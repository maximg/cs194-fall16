
{-# LANGUAGE RankNTypes #-}

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

data Lens a b = Lens { view :: a -> b
                     , over :: (b -> b) -> (a -> a)
                     , overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
                     }

point :: Lens Atom Point
point = mkLens _point setPoint
element :: Lens Atom String
element = mkLens _element setElement
x, y :: Lens Point Double
x = mkLens _x setX
y = mkLens _y setY


setAtomX' :: Double -> Atom -> Atom
setAtomX' = set (point `comp` x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view over overF
    where
        over f a = set (f (view a)) a
        overF f a = (\b' -> set b' a) <$> f (view a)

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (over l1 . over l2)
                  (overF l1 . overF l2)

set :: Lens a b -> b -> a -> a
set l x = over l (const x)
