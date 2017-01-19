import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import System.Exit

-- http://www.seas.upenn.edu/~cis194/fall16/hw/08-functor-applicative.html

-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor (ComplicatedA a) where
    fmap f (Con1 x y) = Con1 x (f y)
    fmap f (Con2 xs) = Con2 (map go xs) where
        go Nothing = Nothing
        go (Just t) = Just (f . t)

instance Functor g => Functor (ComplicatedB f g a) where
    fmap t (Con3 x) = Con3 x
    fmap t (Con4 x) = Con4 (fmap t x)
    fmap t (Con5 x) = Con5 (fmap (map t) <$> x)

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' xs = (\x -> (x,x)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = (,) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\x _ -> (x,x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (+) <$> ((+1) <$> xs) <*> ((+1) <$> ys)

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = g <$> xs where
    g x = if x > 0 then (x, 0)
                   else (0, x)

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer,Integer)
func7' xs = g <$> xs where
    g x = if x > 0 then (x, 0)
                   else (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

-- func9 requires a Monad, as the function return is a monadic value

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Applicative f => f Integer -> f Integer
func10' xs = (+10) <$> ((*) <$> xs <*> xs)

-- Exercise 3

data Parser a = P (String -> Maybe (a,String))

runParser :: Parser a -> String -> Maybe (a,String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p xs = case runParser p xs of
    Just (x,"") -> Just x
    _           -> Nothing

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser x = P (\xs -> Just(x,xs))

instance Functor Parser where
    fmap f (P g) = P (\xs -> g xs >>= (\(x,xs') -> Just (f x,xs')))

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P p where
        p input = case runParser fp input of
            Nothing -> Nothing
            Just (f, input') -> case runParser fx input' of
                Nothing -> Nothing
                Just (x, input'') -> Just (f x, input'')

instance Monad Parser where
    return = pureParser
    fa >>= k = P p where
        p input = case runParser fa input of
            Nothing -> Nothing
            Just (x, input') -> runParser (k x) input'


anyChar :: Parser Char
anyChar = P p where
    p [] = Nothing
    p (x:xs) = Just (x,xs)

char :: Char -> Parser ()
char c = do
    x <- anyChar
    if x == c then return ()
              else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
    x <- anyChar
    if x == c then noParser
              else return x

orElse :: Parser a -> Parser a -> Parser a
orElse fx fy = P $ \input ->
    case runParser fx input of
        Nothing -> runParser fy input
        Just x -> Just x

many :: Parser a -> Parser [a]
many fx = ((:) <$> fx <*> many fx) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy pa p = ((:) <$> pa <*> many (p >> pa)) `orElse` return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

-- Exercise 4

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

letterOrDigit :: Parser Char
letterOrDigit = do
    x <- anyChar
    if isAlphaNum x then return x
                    else noParser

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

parseINI :: Parser INIFile
parseINI = many1 parseSection
    where
        parseSection = do
            header <- parseHeader
            body <- many parseLine
            return (header, catMaybes body)
        parseHeader = do
            char '['
            header <- parseIdentifier
            char ']'
            char '\n'
            return header
        parseLine = parseDeclaration `orElse` parseComment `orElse` parseEmptyLine
        parseDeclaration = do
            key <- parseIdentifier
            many (char ' ')
            char '='
            many (char ' ')
            value <- many (anyCharBut '\n')
            char '\n'
            return $ Just (key,value)
        parseComment = do
            char '#'
            many (anyCharBut '\n')
            char '\n'
            return Nothing
        parseEmptyLine = do
            char '\n'
            return Nothing
        parseIdentifier = many1 letterOrDigit

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure