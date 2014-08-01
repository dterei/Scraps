{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleContexts #-}
module Main where

import GHC.Generics

data Empty deriving Generic
data Single = Single deriving Generic
data SingleP a = SingleP a deriving Generic

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Generic)

class EncodeG f where
  encodeG :: f p -> [Bool]

instance EncodeG V1 where
  encodeG = undefined

instance EncodeG U1 where
  encodeG = const []

instance (Encode c) => EncodeG (K1 i c) where
  encodeG (K1 c) = encode c

instance (EncodeG f) => EncodeG (M1 i t f) where
  encodeG (M1 c) = encodeG c

instance (EncodeG a, EncodeG b) => EncodeG (a :+: b) where
  encodeG (L1 x) = False : encodeG x
  encodeG (R1 y) = True  : encodeG y

instance (EncodeG a, EncodeG b) => EncodeG (a :*: b) where
  encodeG (x :*: y) = encodeG x ++ encodeG y

class Encode a where
  encode :: a -> [Bool]
  default encode :: (Generic a, EncodeG (Rep a)) => a -> [Bool]
  encode x = encodeG (from x)

instance Encode Empty
instance Encode Single
instance (Encode a) => Encode (SingleP a)
instance (Encode a) => Encode (Tree a)

main = putStrLn "Hello"

