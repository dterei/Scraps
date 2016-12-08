{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}
module P1 where

import Data.Char
import Control.Applicative
import Control.Monad

-- A Parser Type
-- -------------

newtype Parser a = P { runP :: String -> [(a, String)] }

result :: a -> Parser a
result v = P $ \inp -> [(v, inp)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind pa fb = P $
    \inp0 -> [(b, inp2) | (a, inp1) <- runP pa inp0,
                          (b, inp2) <- runP (fb a) inp1]


-- Abstracting sequencing
-- ----------------------

instance Functor Parser where
  fmap f pa = P $ \inp -> [(f a, xs) | (a, xs) <- runP pa inp]

instance Applicative Parser where
  pure = result

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- Parser (a -> b) :: String -> [((a -> b), String)]
  -- Parser a        :: String -> [(a, String)]
  -- Parser b        :: String -> [(b, String)]

  -- choice 1: Input -> P (a -> b) -> P a -> P b
  (<*>) pf pa = P $ \inp -> [ (f a, ys) | (f, xs) <- runP pf inp
                                        , (a, ys) <- runP pa xs ]

  -- choice 2: Input -> P (a -> b) | Input -> P a -> P b
  -- (<*>) pf pa = P $ \inp -> [ (f a, ys) | (a, xs) <- runP pa inp
  --                                       , (f, ys) <- runP pf inp ]

  -- choice 3: Input -> P a -> P (a -> b) -> P b
  -- (<*>) pf pa = P $ \inp -> [ (f a, ys) | (a, xs) <- runP pa inp
  --                                       , (f, ys) <- runP pf xs ]

  -- Applicative laws guide you to choice 1 only.

instance Monad Parser where
  (>>=) = bind


-- Abstracting choice
-- ------------------
--
-- We once again want to generalize these operators to all type constructors,
-- this leads us to the MonadPlus type-class
-- zero :: Parser a
-- plus :: Parser a -> Parser a -> Parser a

zero :: Parser a
zero = P $ \inp -> []

plus :: Parser a -> Parser a -> Parser a
plus pa pb = P $ \inp -> runP pa inp ++ runP pb inp

instance Alternative Parser where
  empty = zero
  (<|>) = plus

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

item :: Parser Char
item = P go
  where
    go (x:xs) = [(x,xs)]
    go []     = []

sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= \c -> if pred c then return c else zero

char :: Char -> Parser Char
char c = sat (== c)

-- | It's worth studying in detail how this works to really help understand
-- monadic and applicative structures / types. We take the string we are
-- matching, and create a Parser Char that matches on the first character, and
-- then we recursively call ourselves to match on the remainder of the string.
--
-- So for the partial application `sstring "abc` we get with unfolding:
--         
--         char 'a' >> char 'b' >> char 'c' >> return "" 
--            >> return "c" >> return "ab" >> return "abc"
--
-- From this we can see visually why these types of parsers are referred to as
-- 'recursive decent parsers'. For the above, since `return x >> return y =
-- return y` this simplifies to:
--
--         char 'a' >> char 'b' >> char 'c' >> return "abc"
--
-- So as long as each `char` parser succeeds, we'll continue to the `return`
-- statement. We don't actually care what the return value of the `char` parser
-- is (although we could use it's return value since `y <- char x: x = y`),
-- just that it succeeds.
--
-- Let's look at how `char` succeeds or fails then:
--
--         char 'a' = sat (== 'a')
--
-- So what does `sat` look like?
--
--         sat (== 'a') = item >>= \c -> if (c == 'a') then return c else mzero
--
-- Now remember, our `Parser` return type is `[(a, String)], where `String` is
-- the remaining input to process (our current pointer) and `a` is the return
-- value type. Importantly though, its a list!
--
-- We use a list as it allows a parser to 'fail' by returning the empty list,
-- or succeed by returning a list with one element, or, for us to combine
-- (`mplus`) multiple parsers and run them in parallel and have multiple
-- results. We typically use this last feature in a manner where we expect one
-- of the parsers to fail and return the empty list, giving us an overall
-- result of a one element list, or empty list if both parsers fail.
-- 
-- Going back to `sat`. The first parser invoked is `item` which for a
-- non-empty input string returns the first character. For an empty input
-- string, `item` fails and returns the empty list. While `item` branches in
-- some sense, it is a primitive. `sat` on the other hand combines `item` and
-- `bind` to produce a branching operation without being a primitive (i.e.,
-- without using the Parser constructor).
--
-- How? Bind gives us access to the value `a` of the previous monadic operation
-- `m a` outside of the type! So that we can decide which monadic operation to
-- run next, and, to do so without constructor access to the type!
--
-- One way to think about this is that a monad has a context of sorts,
-- generally some state passed around, similar to a 'this' pointer in an OO
-- language, with the monadic functions being methods. 'bind' allows us to not
-- just manipulate the 'a' value, but also the 'this' pointer, to replace it
-- with a different context.
--
-- Applicative doesn't allow us to replace the context as our sequencing
-- operations (<*>) all take functions within the context.
--  
string :: String -> Parser String
string "" = return ""
--
-- using bind directly:
string (x:xs) = char x >> string xs >> return (x:xs)
--
-- using monadic comprehension:
-- string (x:xs) = [x:xs | _ <- char x, _ <- string xs]
--
-- using do notation:
-- string (x:xs) = do { char x; string xs; return (x:xs) }

-- | More consice ssat using monad comprehension
sat' :: (Char -> Bool) -> Parser Char
-- ssat' pred = iitem >>= \c -> if pred c then return c else mzero
sat' pred = [ c | c <- item, pred c ]
-- ssat' pred = iitem >>= guard . pred

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

space :: Parser Char
space = sat isSpace

word :: Parser String
word = [ l:ls | l <- letter, ls <- word ] <|> return ""
-- ^ above structure is that we apply the same Parser many times until it no
-- longer succeeds (e.g., run-to-failure). Let's abstract that out:


-- | Repeating actions
-- -------------------

many' :: Parser a -> Parser [a]
many' p = [ l:ls | l <- p, ls <- many p ] <|> return []
-- many :: Alternative f => f a -> f [a]

spaces :: Parser String
spaces = many space

ident :: Parser String
ident = [ x:xs | x <- lower, xs <- many alphanum ]

-- often we want a sequence with at least 1 element (e.g., many = *, many1 = +)
many1 :: Parser a -> Parser [a]
many1 p = [ l:ls | l <- p, ls <- many p ]

nat :: Parser Int
nat = [ read n | n <- many1 digit ]

int :: Parser Int
int = [ -n | _ <- char '-', n <- nat ] <|> nat

int' :: Parser Int
int' = ((char '-' *> pure negate) <|> pure id) <*> nat


-- | Repeating with seperators
-- ---------------------------

ints :: Parser [Int]
ints = [ n:ns | _ <- char '['
              , n <- int
              , ns <- many [ m | _ <- char ',', m <- int ]
              , _ <- char ']' ]

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = [ n:ns | n <- p
                      , ns <- many [ m | _ <- sep, m <- p ] ]

lparen, rparen, lbrack, rbrack, comma :: Parser Char
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
comma = char ','

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket l p r = l *> p <* r

ints' :: Parser [Int]
ints' = bracket lbrack (sepby1 int' comma) rbrack

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep <|> return []

-- | Repeating with meaningul seperators
-- -------------------------------------

-- expr   := expr addop factor | factor
-- addop  := + | -
-- factor := nat | ( expr )

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
factor :: Parser Int

-- Below won't work due to recursive call to expr, need to replace by
-- iteration!
-- expr = factor <|>
--   (do
--     x  <- expr
--     op <- addop
--     y  <- factor
--     return $ x `op` y
--   )
-- Iterative version of expr
-- expr = (factor >>= expr') <|> factor
--   where expr' n = do op <- addop
--                      y  <- factor
--                      expr' (op n y) <|> return (op n y)
-- Removing recursion from iterative version... One nice thing with this
-- version is we don't need the explicit alternatve (<|> factor) case as the
-- use of `many` and `foldl` handles a standalone factor.
expr = [foldl (\x (f,y) -> f x y) x fys
   | x   <- factor
   , fys <- many [(f,y) | f <- addop, y <- factor]
   ]
addop = char '-' *> return (-) <|> char '+' *> return (+)
factor = nat <|> bracket lparen expr rparen

-- Generalizing expr: left-associative
chain1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chain1 px pf =
  [ foldl (\x (f,y) -> f x y) x' fys
  | x'   <- px
  , fys <- many [(f,y) | f <- pf, y <- px]
  ]

expr' :: Parser Int
expr' = chain1 factor addop

-- we can also generalize addop:
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [ pa *> return b | (pa, b) <- xs ]

addop' :: Parser (Int -> Int -> Int)
addop' = ops [ (char '+', (+))
             , (char '-', (-)) ]

-- more efficient version of `chain1` that avoids building an intermediate list
chain1' :: Parser a -> Parser (a -> a -> a) -> Parser a
chain1' px pf = px >>= go
  where
    go x = go1 x <|> return x
    go1 x = do
      f <- pf
      y <- px
      go $ x `f` y 

-- more efficient `nat` using `chain1'`
nat' :: Parser Int
-- nat' = [ read n | n <- many1 digit ]
nat' = [ ord x - ord '0' | x <- digit ] `chain1'` op
  where op :: Parser (Int -> Int -> Int)
        op = return $ \m n -> 10*m + n

-- right associative version of chain1
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 px pf = do
    x <- px
    go x <|> return x
  where
    go x = do
      f <- pf
      y <- chainr1 px pf
      return $ f x y

-- extend expr to support exponent
expr1, term1, factor1 :: Parser Int
addop1, expop1 :: Parser (Int -> Int -> Int)
expr1 = term1 `chain1'` addop1
term1 = factor1 `chainr1` expop1
factor1 = nat' <|> bracket lparen expr1 rparen
addop1 = ops [(char '-', (-)), (char '+', (+))]
expop1 = ops [(char '^', (^))]

-- for completeness
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr px pf v = chainr1 px pf <|> return v

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl px pf v = chain1' px pf <|> return v
