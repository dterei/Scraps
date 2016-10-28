{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

-- derived instances have appropriate super-class constraints
newtype NT m a = NT (m a) deriving (Functor, Applicative, Monad)

main :: IO ()
main = return ()
