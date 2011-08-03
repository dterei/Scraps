module Main where

import B ( a )
import qualified Data.Vector as V

main = do
    let x = (V.empty) :: V.Vector Int
    print $ a
    print $ V.unsafeHead x
    return ()

