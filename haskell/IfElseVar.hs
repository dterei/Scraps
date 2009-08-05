module Main where

import System.IO

main = do
   putStr "Give me some sugar [yn]? "
   hFlush stdout
   sugar <- getChar
   let t = "......."
       ifVar | sugar == 'y' = "Thanks honey!"
             | otherwise = "Go to hell!"
   
   putStrLn (t ++ ifVar)

