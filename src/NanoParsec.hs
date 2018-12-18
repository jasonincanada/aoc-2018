-- This whole file is from http://dev.stephendiehl.com/fun/002_parsers.html
-- I reformatted it a bit for legibility

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [ (f a, s') | (a, s') <- cs s ]

instance Applicative Parser where
  pure = return
  p1 <*> p2 = Parser $ \s -> [ (f a, s2) | (f, s1) <- parse p1 s,
                                           (a, s2) <- parse p2 s1 ]

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

failure :: Parser a
failure = Parser (\_ -> [])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item `bind` \c ->
  if f c
  then unit c
  else (Parser (\cs -> []))

char :: Char -> Parser Char
char c = satisfy (c ==)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s 

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = char ' '

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c;
                     string cs;
                     return (c:cs) }

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

-- Copied from https://hackage.haskell.org/package/parser-combinators-0.2.0/docs/src/Control-Applicative-Combinators.html#sepBy
sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

run :: Parser a -> String -> a
run m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream"
    _           -> error "Parser error."

