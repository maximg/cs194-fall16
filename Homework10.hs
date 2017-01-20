
import Test.QuickCheck

-- http://www.seas.upenn.edu/~cis194/fall16/hw/10-testing.html

-- Exercise 2

data Stream a = Cons a (Stream a) | Empty

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show xs = show $ take 20 $ streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = go 0 where go n = streamInterleave (streamRepeat n) (go (n+1))
ruler1 = streamInterleave (streamRepeat 0) (streamMap (+1) ruler1)

-- Exercise 3

data Supply s a = S (Stream s -> (a, Stream s))


get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S(\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S g) = S (\xs -> let (x,xs') = g xs in (f x,xs'))

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S g1) (S g2) = S (\xs -> let
    (x1, xs') = g1 xs
    (x2, xs'') = g2 xs'
    in (f x1 x2, xs''))


bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S g) f = S (\xs -> let
    (x, xs') = g xs
    (S g') = f x
    in g' xs')

runSupply :: Stream s -> Supply s a -> a
runSupply xs (S g) = fst $ g xs

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply


labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node l r) = Node <$> go l <*> go r
    go (Leaf _) = Leaf <$> get

-- Exercise 1

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = sized $ \size -> do
  frequency [ (1, Leaf <$> arbitrary)
            , (size, do t1 <- resize (size - 1) genTree
                        t2 <- resize (size - 1) genTree
                        return (Node t1 t2) )]

size :: Tree a -> Int
size (Leaf _) = 1
size (Node t1 t2) = size t1 + size t2

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node t1 t2) = toList t1 ++ toList t2

prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = (size t) == length (toList t)

prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = (size $ labelTree t) == size t

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..(toInteger (size t)) - 1]

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = (labelTree . labelTree) t == labelTree t

