{-# LANGUAGE GADTs #-}
module AParser (Parser, runParser, oneOrMore, spaces, ident, satisfy, char, stringParser, parseNum, string, posInt, Functor (..), Applicative(..), Alternative(..)) where


import           Data.Char
import           Control.Applicative 
import           Control.Monad
-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on an empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")
-}


-- For convenience, I've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


-- TODO New work is here: 

stringParser :: Parser String
stringParser = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isAlpha xs

string :: String -> Parser String
string [] = pure []
string (c:cs) = char c *> string cs 

-- Returns nothing if contains anything other than digits + one instance of '.' 
parseDouble :: Parser Double
parseDouble = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = if countLetters (fst (numSpan xs)) '.' > 1 
                          then span isDigit xs 
                          else numSpan xs

-- Dumb helper function to return the full span of a double 
numSpan :: [Char] -> ([Char], [Char])
numSpan as = span (\c -> isDigit c || (c == '.')) as

--https://stackoverflow.com/questions/19297059/count-number-of-instances-of-char-in-a-given-string-haskell
countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

parseNum :: Parser Double
parseNum = ((\c i -> i * (-1)) <$> char '-' <*> parseDouble) <|> parseDouble 


------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

newtype Parser a where
  Parser :: (String -> Maybe (a, String)) -> Parser a

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser f) = f

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap f (Parser x) = Parser (\s -> fmap (first f) (x s))

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s)) 
  (Parser f) <*> (Parser g) = Parser (\s -> case f s of 
                              Nothing -> Nothing  
                              (Just (b,t)) -> fmap (first b) (g t))

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser' :: Parser () 
abParser' = (\a b -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a b c -> [a,c]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  (<|>) f g = Parser (\s -> case runParser f s of 
                     Nothing -> (runParser g s) 
                     x -> x)

intOrUppercase :: Parser ()
intOrUppercase = (\a -> ()) <$> posInt <|> ((\a -> ()) <$> satisfy isUpper)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- from your implementation: http://ozark.hendrix.edu/~yorgey/365/static/Monads12S18.hs
spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

ident :: Parser String 
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> many (satisfy isAlphaNum) 



