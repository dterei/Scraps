{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Data.List (intersperse)

newtype Board = Board [[Int]]

example1 :: Board
example1 = Board $
  [ [ 5, 1, 0 ] ++ [ 0, 0, 0 ] ++ [ 0, 3, 0 ]
  , [ 0, 0, 6 ] ++ [ 9, 2, 0 ] ++ [ 7, 0, 0 ]
  , [ 0, 8, 7 ] ++ [ 1, 0, 0 ] ++ [ 0, 0, 4 ]
  ] ++
  [ [ 7, 0, 0 ] ++ [ 4, 0, 2 ] ++ [ 0, 9, 6 ]
  , [ 0, 0, 0 ] ++ [ 0, 3, 0 ] ++ [ 0, 0, 0 ]
  , [ 4, 6, 0 ] ++ [ 5, 0, 9 ] ++ [ 0, 0, 8 ]
  ] ++
  [ [ 8, 0, 0 ] ++ [ 0, 0, 5 ] ++ [ 1, 6, 0 ]
  , [ 0, 0, 9 ] ++ [ 0, 6, 7 ] ++ [ 4, 0, 0 ]
  , [ 0, 3, 0 ] ++ [ 0, 0, 0 ] ++ [ 0, 8, 7 ]
  ]

instance Show Board where
  show :: Board -> String
  show (Board bd) = concat $ intersperse "\n" $ map show bd

type Possible = [Int]
type WorkingBoard = [[Possible]]

main :: IO ()
main = do
  print example1
  print "============="
  print $ newGame example1


newGame :: Board -> WorkingBoard
newGame (Board bd) = map (map convertCell) bd
  where
    convertCell 0 = [1..9]
    convertCell x = [x]

onePass :: WorkingBoard -> WorkingBoard
onePass = undefined


