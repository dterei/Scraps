-- | Variations of `splitAt` to demonstrate strictness, laziness and lazy
-- pattern matching.
module T where

-- incorrect strict implementation.
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' n []          = ([], [])
splitAt' n (x:xs)      = case splitAt' (n-1) xs of
                                (as,bs) -> (x:as, bs)

-- correct lazy implementation using let.
splitAt_ :: Int -> [a] -> ([a], [a])
splitAt_ n xs | n <= 0 = ([], xs)
splitAt_ n []          = ([], [])
splitAt_ n (x:xs)      = let r = splitAt_ (n-1) xs
                         in (x : fst r, snd r)
                         -- you can see clearly here why the lazy version is
                         -- faster on cases like `head $ fst $ splitAt_ 1000000
                         -- $ repeat 'a'`. All we need to do is evaluate the
                         -- first iteration of the split, not the whole split
                         -- as the rest of the two lists returned are
                         -- generators to continue `splitAt_`

-- correct lazy implementation using lazy patterns.
splitAtt :: Int -> [a] -> ([a], [a])
splitAtt n xs | n <= 0 = ([], xs)
splitAtt n []          = ([], [])
splitAtt n (x:xs)      = case splitAtt (n-1) xs of
                               ~(as, bs) -> (x : as, bs)

-- note: lazy pattern matches on types with more than one constructor are
-- generally dangerous as a lazy pattern always matches and so will potentially
-- lead to a mismatched case error when eventually forced.
