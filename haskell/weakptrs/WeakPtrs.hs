module Main (main) where

import System.Mem
import System.Mem.Weak

data K = K String deriving Show
data V = V String deriving Show

mkW :: String -> String -> IO (Weak V)
mkW ks vs = do
  let k = K ks
      v = V vs
  mkWeak k v Nothing

main :: IO ()
main = do
  wptr <- mkW "k1" "v1"
  v' <- deRefWeak wptr
  case v' of
    Nothing -> putStrLn "Value is: dead!"
    Just v -> putStrLn $ "Value is: " ++ show v
  performMajorGC
  v' <- deRefWeak wptr
  case v' of
    Nothing -> putStrLn "Value is: dead!"
    Just v -> putStrLn $ "Value is: " ++ show v
