module HugeFun where

hugeFun :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
hugeFun a b c d e f g
    = if a == 1
         then sum [a,b,c,d,e,f,g]
         else sum [-a,b,c,d,e,f,g*2]

