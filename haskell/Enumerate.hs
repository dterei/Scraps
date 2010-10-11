module Enumerate where

-- [1,2,3] => [[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
enum :: [Int] -> [[Int]]
enum    []  = [[]]
enum (x:xs) = later ++ (map (x:) later)
    where later = enum xs

