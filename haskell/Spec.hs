module Spec where

f :: Eq a => a -> a -> b -> b -> b
f l r t f = case l == r of
                 True  -> t
                 False -> f

{-# SPECIALISE f :: Int -> Int -> b -> b -> b #-}
