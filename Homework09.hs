import Data.Char
import Data.Maybe
import Data.List

import System.Environment
import System.IO
import System.Exit


-- warm-up mini grammar for sequences like aabab(a(b))

type AB = [ABAtom]
data ABAtom = LitA | LitB | Inner AB
    deriving Show


parseAB :: Descr f => f AB
parseAB = nonTerminal "ab" $
    many1 abAtom

abAtom :: Descr f => f ABAtom
abAtom = nonTerminal "atom" $
    eLitA `orElse` eLitB `orElse` eInner

eLitA :: Descr f => f ABAtom
eLitA = nonTerminal "litA" $
    LitA <$ char 'a'

eLitB :: Descr f => f ABAtom
eLitB = nonTerminal "litB" $
    LitB <$ char 'b'

eInner :: Descr f => f ABAtom
eInner =
    Inner   <$  char '('
            <*> parseAB
            <*  char ')'






-- BNF grammar

parseBNF :: Descr f => f BNF
parseBNF = many1 aProduction

aProduction :: Descr f => f Production
aProduction = nonTerminal "production" $
    (,)    <$> aIdent
           <*  spaces
           <*  char '='
           <*  spaces
           <*> aRHS
           <*  char ';'
           <*  spaces

aRHS :: Descr f => f RHS
aRHS = nonTerminal "rhs" $
    aChoice

aChoice :: Descr f => f RHS
aChoice = nonTerminal "choice" $
    mkChoices <$> aSequence
              <*> many (spaces *> char '|' *> spaces *> aSequence)
              <*  spaces

aSequence :: Descr f => f RHS
aSequence = nonTerminal "sequence" $
    mkSequences <$> aAtom
                <*> many (spaces *> char ',' *> spaces *> aAtom)
                <*  spaces

-- atom = terminal | non-terminal | option | repetition | group;
aAtom :: Descr f => f RHS
aAtom = nonTerminal "atom" $
    aTerminal    `orElse`
    aNonTerminal `orElse`
    aOption      `orElse`
    aRepetition  `orElse`
    aGroup

aTerminal :: Descr f => f RHS
aTerminal = nonTerminal "terminal" $
    Terminal <$  char '\''
             <*> many (aQuotedChar)
             <*  char '\''
             <*  spaces

-- non-terminal = identifier, spaces;
aNonTerminal :: Descr f => f RHS
aNonTerminal = nonTerminal "non-terminal" $
    NonTerminal <$> aIdent
                <*  spaces

-- option = '[', spaces, rhs, spaces, ']', spaces;
aOption :: Descr f => f RHS
aOption = nonTerminal "option" $
    Optional <$  char '['
             <*  spaces
             <*> aRHS
             <*  spaces
             <*  char ']'
             <*  spaces

-- repetition = '{', spaces, rhs, spaces, '}', spaces;
aRepetition :: Descr f => f RHS
aRepetition = nonTerminal "repetition" $
    Repetition <$  char '{'
               <*  spaces
               <*> aRHS
               <*  spaces
               <*  char '}'
               <*  spaces

-- group = '(', spaces, rhs, spaces, ')', spaces;
aGroup :: Descr f => f RHS
aGroup = nonTerminal "group" $
    id <$  char '('
       <*  spaces
       <*> aRHS
       <*  spaces
       <*  char ')'
       <*  spaces

aQuotedChar :: Descr f => f Char
aQuotedChar = nonTerminal "quoted-char" $
    notQuoteOrBackslash                     `orElse`
    (char '\\' *> char '\\' *> pure '\\')   `orElse`
    (char '\\' *> char '\'' *> pure '\'')


aIdent :: Descr f => f String
aIdent = nonTerminal "identifier" $
    (:) <$> letter
        <*> many (letter `orElse` digit `orElse` (char '-' *> pure '-'))


-- Example: Simple expressions:

data Expr = Plus Expr Expr | Mult Expr Expr | Const Integer
    deriving Show

mkPlus :: Expr -> [Expr] -> Expr
mkPlus = foldl Plus

mkMult :: Expr -> [Expr] -> Expr
mkMult = foldl Mult

parseExp :: Descr f => f Expr
parseExp = nonTerminal "expr" $
    ePlus

ePlus :: Descr f => f Expr
ePlus = nonTerminal "plus" $
    mkPlus <$> eMult
           <*> many (spaces *>  char '+' *>  spaces *> eMult)
           <*  spaces

eMult :: Descr f => f Expr
eMult = nonTerminal "mult" $
    mkPlus <$> eAtom
           <*> many (spaces *>  char '*' *>  spaces *> eAtom)
           <*  spaces

eAtom :: Descr f => f Expr
eAtom = nonTerminal "atom" $
    aConst `orElse` eParens parseExp

aConst :: Descr f => f Expr
aConst = nonTerminal "const" $ Const . read <$> many1 digit

eParens :: Descr f => f a -> f a
eParens inner =
    id <$  char '('
       <*  spaces
       <*> inner
       <*  spaces
       <*  char ')'
       <*  spaces




-- EBNF in Haskell

data RHS
  = Terminal String
  | NonTerminal String
  | Choice RHS RHS
  | Sequence RHS RHS
  | Optional RHS
  | Repetition RHS
  deriving (Show, Eq)

mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

ppRHS :: RHS -> String
ppRHS = go 0
  where
    go _ (Terminal s)     = surround "'" "'" $ concatMap quote s
    go _ (NonTerminal s)  = s
    go a (Choice x1 x2)   = p a 1 $ go 1 x1 ++ " | " ++ go 1 x2
    go a (Sequence x1 x2) = p a 2 $ go 2 x1 ++ ", "  ++ go 2 x2
    go _ (Optional x)     = surround "[" "]" $ go 0 x
    go _ (Repetition x)   = surround "{" "}" $ go 0 x

    surround c1 c2 x = c1 ++ x ++ c2

    p :: Int -> Int -> String -> String
    p a n | a > n     = surround "(" ")"
          | otherwise = id

    quote '\'' = "\\'"
    quote '\\' = "\\\\"
    quote c    = [c]

type Production = (String, RHS)
type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i,rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

-- The parser

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
    Just (result, "") -> Just result
    _ -> Nothing -- handles both no result and leftover input

noParserP :: Parser a
noParserP = P (\_ -> Nothing)

pureParserP :: a -> Parser a
pureParserP x = P (\input -> Just (x,input))

instance Functor Parser where
    fmap f p = P p'
      where
        p' input = case runParser p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParserP
    p1 <*> p2 = P $ \input -> do
        (f, rest1) <- runParser p1 input
        (x, rest2) <- runParser p2 rest1
        return (f x, rest2)

instance Monad Parser where
    return = pure
    p1 >>= k = P $ \input -> do
        (x, rest1) <- runParser p1 input
        runParser (k x) rest1

anyCharP :: Parser Char
anyCharP = P $ \input -> case input of
    (c:rest) -> Just (c, rest)
    []       -> Nothing

charP :: Char -> Parser ()
charP c = do
    c' <- anyCharP
    if c == c' then return ()
               else noParserP

anyCharButP :: Char -> Parser Char
anyCharButP c = do
    c' <- anyCharP
    if c /= c' then return c'
               else noParserP

letterOrDigitP :: Parser Char
letterOrDigitP = do
    c <- anyCharP
    if isAlphaNum c then return c else noParserP

orElseP :: Parser a -> Parser a -> Parser a
orElseP p1 p2 = P $ \input -> case runParser p1 input of
    Just r -> Just r
    Nothing -> runParser p2 input

manyP :: Parser a -> Parser [a]
manyP p = ((:) <$> p <*> manyP p) `orElseP` return []


-- A grammar-producing type constructor

newtype Grammar a = G ([String] -> (BNF, RHS))

runGrammer :: String -> Grammar a -> BNF
runGrammer main (G g) = runGrammer' main (g []) where
    runGrammer' main (prods, NonTerminal nt) | main == nt = prods
    runGrammer' main (prods, rhs) = prods ++ [(main, rhs)]


ppGrammar :: String -> Grammar a -> String
ppGrammar main g = ppBNF $ runGrammer main g

charG :: Char -> Grammar ()
charG c = G $ \_ -> ([], Terminal [c])

anyCharG :: Grammar Char
anyCharG = G $ \_ -> ([], NonTerminal "char")

manyG :: Grammar a -> Grammar [a]
manyG (G g) = G $ \seen ->
    let (prods,rhs) = g seen
    in (prods, Repetition rhs)

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G g1) (G g2) = G $ \seen ->
    let (prods1, rhs1) = g1 seen
        (prods2, rhs2) = g2 seen
    in (mergeProds prods1 prods2, Choice rhs1 rhs2)

instance Functor Grammar where
    fmap _ (G bnf) = G bnf

instance Applicative Grammar where
    pure x = G $ \_ -> ([], Terminal "")
    G g1 <*> G g2 = G $ \seen -> makeG (g1 seen) (g2 seen) where
        makeG (prods1, Terminal "") (prods2, rhs2) = (mergeProds prods1 prods2, rhs2)
        makeG (prods1, rhs1) (prods2, Terminal "") = (mergeProds prods1 prods2, rhs1)
        makeG (prods1, rhs1) (prods2, rhs2) = (mergeProds prods1 prods2, Sequence rhs1 rhs2)

primitiveG :: String -> Grammar a
primitiveG s = G $ \_ -> ([], NonTerminal s)

newlineG :: Grammar ()
newlineG = primitiveG "newline"

nonTerminalG :: String -> (Grammar a) -> Grammar a
nonTerminalG name (G g) = G $ \seen ->
    if name `elem` seen
    then ([], NonTerminal name)
    else let (prods, rhs) = g (name : seen)
         in (prods ++ [(name, rhs)], NonTerminal name)


-- The generic approach

class Applicative f => Descr f where
    char :: Char -> f ()
    many :: f a -> f [a]
    orElse :: f a -> f a -> f a
    primitive :: String -> Parser a -> f a
    nonTerminal :: String -> f a -> f a

instance Descr Parser where
    char = charP
    many = manyP
    orElse = orElseP
    primitive _ p = p
    nonTerminal _ p = p

instance Descr Grammar where
    char = charG
    many = manyG
    orElse = orElseG
    primitive s _ = primitiveG s
    nonTerminal main g = nonTerminalG main g

many1 :: Descr f => f a -> f [a]
many1 p = pure (:) <*> p <*> many p

sepBy :: Descr f => f a -> f () -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 *> p1))) `orElse` pure []

newline :: Descr f => f ()
newline = primitive "newline" (charP '\n')

anyChar :: Descr f => f Char
anyChar = primitive "char" anyCharP

letter :: Descr f => f Char
letter = primitive "letter" $ do
    c <- anyCharP
    if isLetter c then return c else noParserP

digit :: Descr f => f Char
digit = primitive "digit" $ do
    c <- anyCharP
    if isDigit c then return c else noParserP

notQuoteOrBackslash :: Descr f => f Char
notQuoteOrBackslash = primitive "non-quote-or-backslash" $ do
    c <- anyCharP
    if c `notElem` ['\\','\''] then return c else noParserP

spaces :: Descr f => f ()
spaces = nonTerminal "spaces" $
    () <$ many (char ' ' `orElse` newline)



-- The main function

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStr $ ppGrammar "bnf" parseBNF
        [fileName] -> do
            input <- readFile fileName
            case parse parseBNF input of
                Just i -> putStr $ ppBNF i
                Nothing -> do
                    hPutStrLn stderr "Failed to parse INI file."
                    exitFailure
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
