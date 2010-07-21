module Main where

import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

bench_foldl xs = foldl (\x y -> x ++ [y]) [] xs

bench_foldl2 xs = foldl (\x y -> y:x) [] xs

bench_foldl3 xs = foldl (\x y -> y:x) [] (reverse xs)

bench_foldl4 xs = foldl' (\x y -> y:x) [] (reverse xs)

bench_foldr xs = foldr (\x y -> x:y) [] xs

main = do
    let mAX = 50000

    putStrLn "Testing perf of foldl and foldr using '++' and ':' for list concat respectively"

    putStrLn "Running foldl and ':'"
    time $ length (bench_foldl2 [1..1000000]) `seq` return()
    time $ length (bench_foldl2 [1..1000000]) `seq` return()

    putStrLn "Running foldl (reverse) and ':'"
    time $ length (bench_foldl3 [1..1000000]) `seq` return()
    time $ length (bench_foldl3 [1..1000000]) `seq` return()

    putStrLn "Running foldl' (reverse) and ':'"
    time $ length (bench_foldl4 [1..1000000]) `seq` return()
    time $ length (bench_foldl4 [1..1000000]) `seq` return()

    putStrLn "Running foldr and ':'"
    time $ length (bench_foldr [1..1000000]) `seq` return()
    time $ length (bench_foldr [1..1000000]) `seq` return()

    putStrLn "Running foldl and '++'"
    time $ length (bench_foldl [1..10000]) `seq` return()

    putStrLn "Test done!"
    return ()

