{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
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

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

{-exercise 1-}
updateFirst :: (a -> b) -> (a, c) -> (b, c)
updateFirst f ac = (f (fst ac), snd ac)

instance Functor Parser where
    {-fmap :: (a -> b) -> Parser a -> Parser b-}
    fmap f p = Parser runP
        where
            runP xs = fmap (updateFirst f) (runParser p xs)

{-exercise 2-}
instance Applicative Parser where
    pure a = Parser f
        where
            f xs = Just (a, xs)
    (<*>) pf pa = Parser runP
        where
            runP xs = case (runParser pf xs) of
                Nothing -> Nothing
                Just (fun, ys) -> runParser (fmap fun pa) ys

{-exercise 3-}
aParser :: Parser (Char -> (Char, Char))
aParser = fmap (,) (char 'a')

bParser :: Parser Char
bParser = char 'b'

abParser :: Parser (Char, Char)
abParser = (<*>) aParser bParser

abParser_ :: Parser (())
abParser_ = fmap (\x -> ()) abParser

intSpace :: Parser Integer
intSpace = const <$> posInt <*> (char ' ')

intPair :: Parser (Integer, Integer)
intPair = (fmap (,) intSpace) <*> posInt

{-exercise 4-}
instance Alternative Parser where
    empty = Parser runP
        where runP xs = Nothing
    (<|>) fa fa' = f a
