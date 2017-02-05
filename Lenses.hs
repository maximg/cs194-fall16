
{-# LANGUAGE RankNTypes #-}

import Control.Applicative

-- http://www.seas.upenn.edu/~cis194/fall16/lectures/14-lenses.html

data Atom = Atom { _element :: String, _point :: Point } deriving Show
data Point = Point { _x :: Double, _y :: Double } deriving Show

at = Atom "atom" (Point 55 66)
pt = Point 77 88

setPoint :: Point -> Atom -> Atom
setPoint p a = a { _point = p }
setElement :: String -> Atom -> Atom
setElement e a = a { _element = e }
setX, setY:: Double -> Point -> Point
setX x p = p { _x = x }
setY y p = p { _y = y }


getAtomX :: Atom -> Double
getAtomX = _x . _point

setAtomX :: Double -> Atom -> Atom
setAtomX x a = setPoint (setX x (_point a)) a

type Lens a b = forall t. Functor t => (b -> t b) -> (a -> t a)

point :: Lens Atom Point
point = mkLens _point setPoint
element :: Lens Atom String
element = mkLens _element setElement
x, y :: Lens Point Double
x = mkLens _x setX
y = mkLens _y setY


setAtomX' :: Double -> Atom -> Atom
setAtomX' = set (point . x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = overF
    where overF f a = (\b' -> set b' a) <$> f (view a)

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = l1 . l2

view :: Lens a b -> a -> b
view l a = unC $ l MkC a

set :: Traversal a b -> b -> a -> a
set l x = over l (const x)

over :: Traversal a b -> (b -> b) -> (a -> a)
over l f a = unI $ l f' a
    where f' b = MkI (f b)

--listOf :: Traversal a b -> a -> [a]
--listOf l a = unCL $ l MkCL a

newtype I x = MkI x
unI :: I x -> x
unI (MkI x) = x

instance Functor I where
  fmap f x = MkI (f (unI x))

newtype C b x = MkC b

unC :: C b x -> b
unC (MkC b) = b

instance Functor (C b) where
  fmap f (MkC b) = MkC b

type Traversal a b = forall t . Applicative t => (b -> t b) -> (a -> t a)

instance Applicative I where
  pure x = MkI x
  f <*> x = MkI $ (unI f) (unI x)

-- does not compile, not sure why

newtype CL b x = MkCL [b]

unCL :: CL b x -> [b]
unCL (MkCL b) = b

instance Functor CL where
  fmap f (MkCL a) = MkCL (map f a)

instance Applicative (CL b) where
  pure _ = MkCL []
  MkCL bs1 <*> MkCL bs2 = MkCL (bs1 ++ bs2)

