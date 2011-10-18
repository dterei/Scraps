{-# LANGUAGE ExistentialQuantification #-}

module Main where

data Any = forall a . (Show a) => Any a

instance Show Any where
    show (Any a) = show a

main = do
    let xs = [Any 'h', Any (1::Int), Any 2.33, Any "sss"]
    mapM_ print xs
    return ()

