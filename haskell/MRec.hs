{-# LANGUAGE RecursiveDo #-}
module MRec where

import Data.Monoid
import Data.IORef
import Control.Monad.Fix

data Node = Node Int (IORef Node)

-- non-recursive, use undefined
mkNode1 :: Int -> IO (IORef Node)
mkNode1 n = do
  p <- newIORef undefined
  let node = Node n p
  writeIORef p node
  putStrLn $ "Created Node: " <> show n
  return p

-- recurisve, using rec notation
mkNode2 :: Int -> IO (IORef Node)
mkNode2 n = do
  rec p <- newIORef (Node n p)
  putStrLn $ "Created Node: " <> show n
  return p

-- fix-point using mfix
mkNode3 :: Int -> IO (IORef Node)
mkNode3 n = mfix $ \p -> do
  p' <- newIORef (Node n p)
  putStrLn $ "Created Node: " <> show n
  return p'

-- recurisve, using mdo notation
mkNode4 :: Int -> IO (IORef Node)
mkNode4 n = mdo
  p <- newIORef (Node n p)
  putStrLn $ "Created Node: " <> show n
  return p

main :: IO ()
main = do
  n1p <- mkNode4 0
  Node x p <- readIORef n1p
  putStrLn $ "Node: " <> show x
  return ()

