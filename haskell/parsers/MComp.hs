{-# LANGUAGE MonadComprehensions #-}
module MComp where

import Control.Applicative

data Opt a = Yes a | No deriving Show

instance Functor Opt where
  fmap f No = No
  fmap f (Yes a) = Yes $ f a

instance Applicative Opt where
  pure = Yes
  No <*> _ = No
  (Yes f) <*> a = fmap f a

instance Monad Opt where
  (Yes a) >>= f = f a
  No >>= _ = No

instance Alternative Opt where
  empty = No
  No <|> b = b
  a <|> _ = a

mcomp1 :: Opt Int
mcomp1 = [ x + y | x <- genX, y <- genY ]
  where genX = Yes 1
        genY = Yes 2

mcomp2 :: Opt Int
mcomp2 = [ x + y | x <- genX, y <- genY ]
  where genX = Yes 1
        genY = No

-- mcomp3 & mcomp4 require and Alternative instance
mcomp3 :: Opt Int
mcomp3 = [ x + y | x <- genX, y <- genY, x > 0, y > 0 ]
  where genX = Yes 1
        genY = Yes 2

mcomp4 :: Opt Int
mcomp4 = [ x + y | x <- genX, y <- genY, x < 0, y > 0 ]
  where genX = Yes 1
        genY = Yes 2

