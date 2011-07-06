{-# LANGUAGE MagicHash #-}
module Doublee where

import GHC.Exts

f :: Double# -> Double
f x = D# (x +## 1.324##)

