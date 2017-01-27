{-# LANGUAGE FlexibleInstances, GADTs #-}
module Main (main) where

newtype Pixels = Pixels { fromPixels :: Integer } deriving Show

instance (a ~ b) => Num ((Integer -> a) -> b) where
  fromInteger x f = f x

-- fromInteger :: Integer -> a
-- fromInteger :: Integer -> ((Integer -> a) -> b)
-- fromInteger :: Integer -> (Integer -> a) -> b
--
-- Pixels :: Integer -> Pixels
--
-- fromInteger 320 :: (Integer -> a) -> b
-- fromInteger 320 Pixels :: (Pixels ~ b) => b

width, height :: Pixels
width = 320 Pixels
height = 320 Pixels

main :: IO ()
main = print width
