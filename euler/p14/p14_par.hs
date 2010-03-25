import Control.Parallel
import Data.Word

-- Gain a lot of speed here using Word32 type and `mod` for parity checking
-- instead of `even`. This is all since it keeps the values unboxed.
collatzLen :: Int -> Word32 -> Int
collatzLen c 1 = c
collatzLen c n = collatzLen (c+1) $ if n `mod` 2 == 0 then n `div` 2 else 3*n+1

pmax x n = x `max` (collatzLen 1 n, n)

solve xs = foldl pmax (1,1) xs

main = print soln
    where
        s1 = solve [2..500000]
        s2 = solve [500001..999999]
        soln = s2 `par` (s1 `pseq` max s1 s2)

