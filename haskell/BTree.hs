module BTree where

import Data.List (sort)

data BTree a = BTree { minNodes :: Int, node :: [a], children :: [BTree a] }
  deriving (Show, Eq)

btNew :: (Ord a) => Int -> BTree a
btNew d = BTree d [] []

btInsert :: (Ord a) => a -> BTree a -> BTree a
btInsert v (BTree d n [])
  | length n < d = BTree d (sort $ v:n) []
  | otherwise    = BTree d [mid] [btLow, btHig]
      where (low, mid, hig) = breakUp (d `quot` 2 + 1) (sort $ v:n)
            btLow = BTree d low []
            btHig = BTree d hig []
-- btInsert v (BTree d n c)

breakUp :: Int -> [a] -> ([a], a, [a])
breakUp x xs = (low, mid, high)
  where (low, hi) = splitAt (x-1) xs
        mid       = head hi
        high      = drop 1 hi

