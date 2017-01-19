
-- http://www.seas.upenn.edu/~cis194/fall16/hw/07-laziness.html

import System.Random
import Control.Monad

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- naive fib, sloooooow
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- proper fib, fasssst
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

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


data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node l r) = Node <$> go l <*> go r
    go (Leaf _) = Leaf <$> get

-- Demo

type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
    let (roll, gen') = randomR (1,6) gen
    in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
    stdGen <- getStdGen
    let diceRolls = randomDice stdGen
    return $ runSupply diceRolls r

-- MG: very confusing that this looks like a const (output only)
-- but it actually is not - it takes it's input in hidden fashion
-- from the monadic context
averageOfTwo :: Rand Double
averageOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral (d1 + d2) / 2

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral $ if (d1 > d2) then d1 else d2

-- Look, ma, I’m recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
    d <- get
    if (d == 1) then return 0
                else do s <- sumUntilOne
                        return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
    samples <- replicateM n what
    return (maximum samples, sum samples / fromIntegral n)

main = mapM_ go [ ("average of two", averageOfTwo)
            , ("bestOutOfTwo",   bestOutOfTwo)
            , ("sumUntilOne",    sumUntilOne)
            ]
  where
    n = 10000
    go (name, what) = do
    (max, avg) <- runRand (sample n what)
    putStrLn $ "Playing \"" ++ name ++ "\" " ++ show n ++ " times " ++
               "yields a max of " ++ show max ++ " and an average of " ++
           show avg ++ "."