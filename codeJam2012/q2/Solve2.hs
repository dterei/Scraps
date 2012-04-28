module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO

type Triplet = (Int, Int, Int)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ die "Usage: Solve2 <file>"
    contents <- lines `fmap` readFile (args !! 0)
    let scores = map (\l -> map readi $ words l) $ tail contents
        solns  = map solveG scores
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

solveG :: [Int] -> Int
solveG (g : s : p : scores') = solve 0 s scores'
  where
    solve soln _ [] = soln
    solve soln sup (score : scores) =
        let (s1, s2) = genPossibleTriplets score
        in case (good s1, good s2) of
            (True, _) | not (surprising s1) -> solve (soln + 1) sup scores
            (_, True) | not (surprising s2) -> solve (soln + 1) sup scores
            (True, _) | sup > 0             -> solve (soln + 1) (sup - 1) scores
            (_, True) | sup > 0             -> solve (soln + 1) (sup - 1) scores
            _                               -> solve soln sup scores

    good s = tripletValid s && best s >= p
    

genPossibleTriplets :: Int -> (Triplet, Triplet)
genPossibleTriplets n
    | n `mod` 3 == 0
    = let x = floor $ toRational n / 3
      in ((x, x, x), (x - 1, x, x + 1))

    | (n - 1) `mod` 3 == 0
    = let x = floor $ toRational (n - 1) / 3
      in ((x, x, x + 1), (x - 1, x + 1, x + 1))
    
    | (n - 2) `mod` 3 == 0
    = let x = floor $ toRational (n - 2) / 3
      in ((x, x, x + 2), (x, x + 1, x + 1))

tripletValid :: Triplet -> Bool
tripletValid (x, y, z)
    | x > 10 || y > 10 || z > 10 || x < 0 || y < 0 || z < 0 = False
    | otherwise = True

best :: Triplet -> Int
best (x, y, z) | x >= y && x >= z = x
               | y >= x && y >= z = y
               | z >= x && z >= y = z

surprising :: Triplet -> Bool
surprising (x, y, z) | abs (x - y) >= 2 = True
                     | abs (x - z) >= 2 = True
                     | abs (y - z) >= 2 = True
                     | otherwise        = False

