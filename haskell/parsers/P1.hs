{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}
module P1 where

import Data.Char
import Control.Applicative
import Control.Monad

-- | Parser type

-- Attempt 1: String to a tree intepretation
data Tree
type Parser1 = String -> Tree

-- Attempt 2: Parser may not consume all input, so return left-over
type Parser2 = String -> (Tree, String)

-- Attempt 3: Parse may fail (return empty list)
type Parser3 = String -> [(Tree, String)]

-- Attempt 4: Different parsers return different trees, abstract
type Parser4 a = String -> [(a, String)]

type Parser a = Parser4 a


-- | Primitive parsers

result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item (x:xs) = [(x,xs)]
item []     = []

-- | Building new parsers -- combinators
-- Take inspiration from BNF, want a sequencing operation and a choice
-- operation.

seq :: Parser a -> Parser b -> Parser (a,b)
seq pa pb = \inp0 -> [((a,b), inp2) | (a, inp1) <- pa inp0,
                                      (b, inp2) <- pb inp1]

-- seq has a problem of creating nested tuples that become difficult to work
-- with, instead want a _monadic_ sequencing, where the sequencing of parsers
-- is integrated with the processing of their values.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind pa fb = \inp0 -> [(b, inp2) | (a, inp1) <- pa inp0,
                                   (b, inp2) <- fb a inp1]

-- we can define 'seq' in terms of 'bind', but not vice-versa
seq2 :: Parser a -> Parser b -> Parser (a,b)
seq2 pa pb = pa `bind` \a -> pb `bind` \b -> result (a,b)

-- choice operator
plus :: Parser a -> Parser a -> Parser a
plus pa1 pa2 = \inp -> pa1 inp ++ pa2 inp


-- | Some simple parsers

-- Note, this holds for an arbitrary monad with a zero value, not just parsers
sat :: (Char -> Bool) -> Parser Char
sat pred = item `bind` \c -> if pred c
                                then result c
                                else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = neWord `plus` result ""
  where neWord = letter `bind` \l -> word `bind` \ls -> result (l:ls)


-- | Monadic parsers
-- result :: a -> Parser a
-- bind :: Parser a -> (a -> Parser b) -> Parser b

-- If we generalize the above constructs to any type constructor M, rather than
-- Parser, we get a monad. A monad technically must satisfy some laws.

class MMonad (m :: * -> *) where
  mreturn :: a -> m a
  mbind :: m a -> (a -> m b) -> m b

-- See P2.hs now...
