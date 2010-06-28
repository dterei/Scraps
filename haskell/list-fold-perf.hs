module Main where

import Text.Printf
import Control.Exception
import System.CPUTime
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

bench_foldl xs = foldl (\x y -> x ++ [y]) [] xs

bench_foldl2 xs = foldl (\x y -> y:x) [] xs

bench_foldr xs = foldr (\x y -> x:y) [] xs

main = do
    putStrLn "Testing perf of foldl and foldr using '++' and ':' for list concat respectively"

    putStrLn "Running foldl and ':'"
    time $ length (bench_foldl2 [1..50000]) `seq` return()

    putStrLn "Running foldl and '++'"
    time $ length (bench_foldl [1..50000]) `seq` return()

    putStrLn "Running foldr and ':'"
    time $ length (bench_foldr [1..50000]) `seq` return()

    putStrLn "Test done!"
    return ()

