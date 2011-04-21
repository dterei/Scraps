module Main where

import Control.Monad
import System.Random

printDiceRolls = do
    gen <- newStdGen
    print $ takeWhile (/=(6::Int)) $ randomRs (1,6) gen

hi5 = endLine $ replicateM_ 5 action
    where action = putStr "Hello"
          endLine a = a >> putChar '\n'

countDown = mapM_ (\n -> putStr $ show n ++ " .. ") [10,9..1] >> putStr "LiftOff!"

