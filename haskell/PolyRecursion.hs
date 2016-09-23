module PolyRecursion where

-- | Polymorphic recursive data type, Nested a -> Nested [a], the type variable
-- changes on recursion.
data Nested a = Cons a (Nested [a]) | End

-- | Need type signature as infering type for polymorphic recursive functions
-- is undecideable.
nlength :: Nested a -> Int
nlength End = 0
nlength (Cons _ xs) = 1 + nlength xs

ex1 = Cons 1 $ Cons [1,2,3] $ Cons [[4],[5],[6]] End

