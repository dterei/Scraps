module Main where

import qualified Data.Maybe as Environment

import qualified System.Environment

main = do
    args <- System.Environment.getArgs
    let b = Just "Hello"
        c = ("a", "b")
    putStrLn $ Environment.fromJust b
    putStrLn $ show $ length args

