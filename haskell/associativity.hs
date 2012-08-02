module Main where

(#+#) :: Int -> Int -> Int
(#+#) = (+)
infixr 9 #+#

(#-#) :: Int -> Int -> Int
(#-#) = (-)
infixl 9 #-#

main :: IO ()
main = do
    let x = 1 #+# 1
        y = 1 #-# 1
        z = 1 #-# 1 #+# 1 -- can't mix since same level but different
                          -- associativity.
    putStrLn $ show x
    putStrLn $ show y
    putStrLn $ show z
    return ()

