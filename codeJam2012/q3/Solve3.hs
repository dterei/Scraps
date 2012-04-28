module Main where

import Control.Monad
import Data.List
import Debug.Trace
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ die "Usage: Solve3 <file>"
    contents <- lines `fmap` readFile (args !! 0)
    let probs = map (\l -> map readi $ words l) $ tail contents
        solns = map solve probs
    mapM_ printSoln $ zip [1..] solns

  where
    readi :: String -> Int
    readi = read

    printSoln :: (Int, Int) -> IO ()
    printSoln (n, soln) = putStrLn $ "Case #" ++ show n ++ ": " ++ show soln

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

solve :: [Int] -> Int
solve [min, max] = go min 0
  where
    go i count
        | i > max   = count
        | otherwise =
            let rs = filter (\x -> x > i && x <= max) $ rotations i
            in go (i + 1) (count + length rs)

solve _ = error "Bad input line! too many numbers"

-- This can give bad rotations (i.e 120 -> 012 -> 12) but
-- we just filter numbers outside the min range so this is OK.
rotations :: Int -> [Int]
rotations i =
    let s = show i
        rotate n = let (x, y) = splitAt n s
                   in read (y ++ x)
    in tail $ nub $ map rotate [0..(length s - 1)]

