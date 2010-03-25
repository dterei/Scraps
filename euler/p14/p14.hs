import Data.Word

-- Gain a lot of speed here using Word32 type and `mod` for parity checking
-- instead of `even`. This is all since it keeps the values unboxed.
collatzLen :: Int -> Word32 -> Int
collatzLen c 1 = c
collatzLen c n = collatzLen (c+1) $ if n `mod` 2 == 0 then n `div` 2 else 3*n+1

pmax x n = x `max` (collatzLen 1 n, n)

main = print . solve $ 1000000
    where solve xs = foldl pmax (1,1) [2..xs-1]

