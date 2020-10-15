{-
MParser.hs
==========
A monadic parsing library, a replication of "Monadic Parsing in Haskell" by
Hutton & Meijer. https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-}

module MParser (module MParser, module Control.Applicative) where

import Control.Applicative
import Data.Char


{- =========================== CORE DEFINITIONS ==============================-}

-- Type for parsers
newtype Parser a = P (String -> [(a,String)])

-- Needed for applicative instance
instance Functor Parser where
  fmap f p = P (\inp -> case parse p inp of
                           [] -> []
                           [(v,out)] -> [(f v, out)])

-- Needed for monad/alternative instance
instance Applicative Parser where
  pure v = P (\inp -> [(v,inp)])
  pf <*> px = P (\inp -> case parse pf inp of
                           [] -> []
                           [(f,out)] -> parse (fmap f px) out)

-- Allows sequencing w/ do notation
instance Monad Parser where
  f >>= g  = P (\inp -> case parse f inp of
                          []       -> []
                          [(v,out)] -> parse (g v) out)

-- Allows sequencing w/ choice operators <|>
instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v,out)] -> [(v,out)])

-- Apply a parser to an input
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- Grabs an item from input stream
item :: Parser Char
item = P (\inp -> case inp of
                    ""     -> []
                    (x:xs) -> [(x,xs)])

look :: Parser ()
look = P (\inp -> case inp of
                    ""     -> []
                    (x:xs) -> [((),(x:xs))])

{- =========================== DERIVED PRIMITIVES ============================-}

-- Make a parser which satisfies a predicate for a character
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- Parse digit
digit :: Parser Char
digit = sat isDigit

-- Parse lowercase character
lower :: Parser Char
lower = sat isLower

-- Parse uppercase character
upper :: Parser Char
upper = sat isUpper

-- Parse letter
letter :: Parser Char
letter = sat isAlpha

-- Parse alphanumerics
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Parse a specific character
char :: Char -> Parser Char
char c = sat (== c)

-- Parse a specific string
string :: String -> Parser String
string []     = return []
string (c:cs) = do x <- char c
                   xs <- string cs
                   return (x:xs)

-- Parse identifiers, a lowercase character followed by alphanumerics
ident :: Parser String
ident = do c <- lower
           cs <- many alphanum
           return (c:cs)

constr :: Parser String
constr = do c <- upper
            cs <- many alphanum
            return (c:cs)

-- Parse natural numbers
nat :: Parser Int
nat = do n <- some digit
         return $ read n

-- Modified isSpace which returns false for newlines
isSpace' :: Char -> Bool
isSpace' c = c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\v'

-- Parse whitespace
space :: Parser ()
space = do many (sat isSpace')
           return ()

allSpace :: Parser ()
allSpace = do many (sat isSpace)
              return ()

line :: Parser ()
line = do symbol "\n"
          return ()

-- Parse integers
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Parse tokens (makes a parser insensitive to whitespace around it)
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Parse identifiers (w/ token)
identifier :: [String] -> Parser String
identifier ks = do x <- token ident
                   if not (elem x ks) then return x
                   else empty

-- Parse constructors
constructor :: Parser String
constructor = token constr

-- Parse natural numbers (w/ token)
natural :: Parser Int
natural = token nat

-- Parse integers (w/ token)
integer :: Parser Int
integer = token int

-- Parse a specific symbol (w/ token)
symbol :: String -> Parser String
symbol s = token (string s)

-- Runs a parser, but ignores usual output
ignore :: Parser a -> Parser ()
ignore p = P (\inp -> case parse p inp of
                      []        -> []
                      [(v,out)] -> [((),out)])

-- Takes a parser and lets you parse if surrounded by parens
parens :: Parser a -> Parser a
parens p = do symbol "("
              v <- p
              symbol ")"
              return v

-- Parse repeated apps of a parser p, separated by a parser sep
sepby :: Parser a -> Parser b -> Parser [a]
pa `sepby` pb = do a  <- pa
                   as <- many (do ignore pb
                                  pa)
                   return (a:as)

-- Parse repeated applications of a parser p, separated by applications of a
-- parser op whose result value is an operator assumed to associate to left,
-- and which combines the results from p parsers
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p
                    rest a
                    where rest a = do f <- op
                                      b <- p
                                      rest (f a b)
                                   <|> return a

-- Parse repeated applications of a parser p, separated by applications of a
-- parser op whose result value is an operator assumed to associate to right,
-- and which combines the results from p parsers
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do a <- p
                    rest a
                    where rest a = do f <- op
                                      b <- p `chainr1` op
                                      return (f a b)
                                   <|> return a

