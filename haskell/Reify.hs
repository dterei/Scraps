{-# LANGUAGE GADTs #-}
module Reify where

-- data NumInst a where
--     MkNumInst :: Num a => NumInst a
-- 
-- intInst :: NumInst Int
-- intInst = MkNumInst
-- 
-- plus :: NumInst a -> a -> a -> a
-- plus MkNumInst p q = p + q
-- 
-- plus2 :: Num a => a -> a -> a
-- plus2 a b = a + b

and :: Class a => a -> a -> a
and x y = classF x y

class Class a where
    classB :: a -> a
    classF :: a -> a -> a

instance Class Int where
    classB 0 = 1
    classB _ = 0

    classF 1 1 = 1
    classF _ _ = 0

instance Class Char where
    classB 'a' = 'b'
    classB  _  = 'a'

    classF 'b' 'b' = 'b'
    classF  _   _  = 'a'

