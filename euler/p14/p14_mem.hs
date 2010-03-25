import Data.Array
import Data.List
import Data.Ord (comparing)

syrs n = a
    where 
        a = listArray (1,n) $ 0:[1 + syr n x | x <- [2..n]]
        syr n x = if x' <= n then a ! x' else 1 + syr n x'
            where 
                x' = if even x then x `div` 2 else 3 * x + 1

main = print $ maximumBy (comparing snd) $ assocs $ syrs 1000000

