-- | Testing what work is shared by GHC. In general you can't assume
-- work will be shared. Sometimes GHC will do CSE and top level
-- floating to share work but not for say papLots which really requires
-- some partial evalutation kind of work. (e.g Max's super evaluator)
module Main where

papLots :: [Double] -> Double -> Double
papLots xs n = n * sum'
    where sum' = foldl ((+) . cos . tan . cos . tan . cos . sin . cos) 0 xs

main :: IO ()
main = do
    let papShared = papLots [1..5000000]
    print "Starting..."
    print "First run..."
    print $ papLots [1..5000000] 2
    print "Second run..."
    print $ papLots [1..5000000] 4
    print "Third run..."
    print $ papShared 3
    print "Fourth run..."
    print $ papShared 5

