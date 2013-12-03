{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module A where

f :: (a ~ b) => a -> b -> a
f a b = b

data MyData = MyData

data family DFTop x y
data instance DFTop MyData MyData = D1 Char
data instance DFTop MyData Char   = D2 Int

class MyClass a where
  data DF a
  foo :: DF a -> Int

instance MyClass Int where
  data DF Int = DI Int
  foo (DI a) = a

ff :: DF Int
ff = DI 1

gg :: Int
gg = foo ff

type family T a
type family TF a b :: * -> *
type family EEq a b :: Bool

type instance TF Int Char = (,) Int

type instance EEq Int Char = False
type instance EEq Int Int  = True

