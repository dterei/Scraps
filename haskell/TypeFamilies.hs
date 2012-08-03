{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
-- | Trying to do CASE 5 from FunDeps.hs but with type families...
module TypeFamilies where

class TCa a where
    type T a :: *
    tcd1 :: a -> T a -> Int
    tcd2 :: a -> Int

instance TCa Int where
    type T Int = Char
    tcd1 n _ = n
    tcd2 _   = 1

-- BAD: As duplicate instance
-- instance TCa Int where
--     type T Bool = Bool
--     tcd1 _ _ = 2
--     tcd2 _   = 3

instance TCa Char where
    type T Char = Int
    tcd1 _ n = n
    tcd2 _   = 1

testFun :: Int
testFun = x + y + z
  where
    x = tcd1 'c' (1 :: Int)
    y = tcd1 (1 :: Int) 'c'
    z = tcd2 'c'

class TCa a => TCb a where
    tcb1 :: a -> Int
    tcb2 :: a -> Int

instance TCb Char where
    tcb1 = flip tcd1 (1 :: Int)
    tcb2 = tcd2

