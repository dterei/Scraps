module Main where

sample = [1..10000]

bench_foldl xs = foldl (\x y -> x ++ [y]) [] xs

bench_foldr xs = foldr (\x y -> x:y) [] xs

main = do
    putStrLn "Testing perf of foldl and foldr using '++' and ':' for list concat respectively"

    putStrLn "Running foldl and '++'"
    let lxs = bench_foldl sample
    putStrLn $ "foldl list: " ++ (show $ length lxs)

    putStrLn "Running foldr and ':'"
    let rxs = bench_foldr sample
    putStrLn $ "foldr list: " ++ (show $ length rxs)

    putStrLn "Test done!"
    return ()

