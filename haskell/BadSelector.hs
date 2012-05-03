module Main where

data A = A {
        aa :: String,
        bb :: Int
    }
    | B {
        aa :: String,
        bb :: Int,
        cc :: Double
        }

main = do
    let a = A "Hello" 1
        b = B "World" 2 2.0
        z = [a,b]
    let f = cc a
    putStrLn $ "f = " ++ (show f)
