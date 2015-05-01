{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Main where

data Boo = Tru | Fal deriving (Show, Eq, Ord)

data T1 a where
  T1 :: a -> T1 a
  deriving (Show, Eq, Ord)

data T2 :: Boo -> * -> * where
  T2 :: Boo -> a -> T2 'Tru a

data Tuple :: (*,*) -> * where
  Tuple :: a -> b -> Tuple '(a,b)

type T3 = Int
type T4 = Boo

-- Can't use T5 anywhere a type with kind :: * is expected, which appears to be
-- anywhere that deals with values.
type T5 = 'Tru

x :: T1 Int
x = T1 1

y :: T1 Boo
y = T1 Tru

z :: T2 'Tru Int
z = T2 Tru 1

w :: Tuple '(Int, Char)
w = Tuple 1 'c'

main :: IO ()
main = do
  print "Hello World"

