module Qs (quicksort) where

main = do
		putStrLn $ (show . quicksort) [1, 4, 2, 9, 10, (-1), 32, 100, 2, 33, 1, 0, (-99)];
		printXxs [9,2,3,4,5,6];
		printXxs2 [9,2,3,4,5,6];
		printXxs3 [9,2,3,4,5,6];
		printXxs4 [9,2,3,4,5,6];

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x]
						++ [x]
						++ quicksort [y | y <- xs, y >= x]

printXxs :: Show a => [a] -> IO ()
printXxs (x:xs) = putStrLn ("X: " ++ show x ++ " Xs: " ++ show xs)

printXxs2 :: Show a => [a] -> IO ()
printXxs2 (x:y) = putStrLn ("X: " ++ show x ++ " Y: " ++ show y)

printXxs3 :: Show a => [a] -> IO ()
printXxs3 (xs:x:x2s) = putStrLn ("Xs: " ++ show xs ++ " X: " ++ show x ++ " X2s: " ++ show x2s)

printXxs4 :: Show a => [a] -> IO ()
printXxs4 (xs:x:[]) = putStrLn ("Xs: " ++ show xs ++ " X: " ++ show x)

