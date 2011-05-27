{-# LANGUAGE TypeSynonymInstances #-}
-- | Make a Num String instance to allow
-- overloading of (+) to be addition and
-- concatenation
module NumString where

import Data.Char
import Data.List

instance Num String where
    (+)         = (++)
    (*) a b     = b ++ a ++ b
    (-) a b     = isSuffixOf b a ? take (length a - length b) a $ a
    negate      = reverse
    abs a       = reverse $ dropWhile isSpace $ reverse (dropWhile isSpace a)
    signum a    = ""
    fromInteger = show

(?) :: Bool -> a -> a -> a
(?) True t _  = t
(?) False _ f = f

