 module StateGame where
 
 import Control.Monad.ST
 import Data.STRef
 
 -- Example use of ST monad
 -- Passes a string of dictionary {a,b,c}
 -- Game is to produce a number from the string.
 -- By default the game is off, a C toggles the
 -- game on and off. A 'a' gives +1 and a b gives -1.
 -- E.g 
 -- 'ab'    = 0
 -- 'ca'    = 1
 -- 'cabca' = 0
 -- State = game is on or off & current score
 --       = (Bool, Int)
 
 playGame :: String -> Int
 playGame s = runST $ do
     ref <- newSTRef (False, 0)
     playGame' ref s
    where
     playGame' ref []     = do
        (_, score) <- readSTRef ref
        return score
 
     playGame' ref (x:xs) = do
         (on, score) <- readSTRef ref
         case x of
              'a' | on -> writeSTRef ref (on, score + 1)
              'b' | on -> writeSTRef ref (on, score - 1)
              'c'      -> writeSTRef ref (not on, score)
              _        -> writeSTRef ref (on, score)
         playGame' ref xs

