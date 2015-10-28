-- https://www.fpcomplete.com/user/bartosz/understanding-algebras
module FAlgebra where

----------------
-- Algebra:
-- (1) A way to construct expressions
-- (2) A way to evaluate expressions (produce a single value)

-- define our expressions (non-recursive)
-- ExprF :: a -> ExprF a
-- ExprF :: * -> *
data ExprF a = Const Int
             | Add a a
             | Mul a a

fixpoint :: (a -> a) -> a
fixpoint f = f valA
  where valA = fixpoint f

-- define our fix-point type-level function
-- Fix :: (a -> a) -> Fix t
-- Fix :: (* -> *) -> *
newtype Fix f = Fx (f (Fix f))

-- Fx :: f (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- define Expr as the fix-point of ExprF
-- Expr = Fx $ ExrFC $ Fx
type Expr = Fix ExprF

-- we can map over expr
-- endofunctor: maps a given category into itself
-- Hask: the category of all Haskell types
instance Functor ExprF where
  fmap _    (Const i) = Const i
  fmap eval (Add l r) = Add (eval l) (eval r)
  fmap eval (Mul l r) = Mul (eval l) (eval r)

-- our evaluator for a single top-level expression
-- picked one type (Int) as our evaluation target (carrier type of the algebra)
alg :: ExprF Int -> Int
alg (Const i) = i
alg (Add l r) = l + r
alg (Mul l r) = l * r

-- hand-rolled eval
evalFull :: Expr -> Int
evalFull (Fx (Const n)) = n
evalFull (Fx (Add l r)) = evalFull l + evalFull r
evalFull (Fx (Mul l r)) = evalFull l * evalFull r

-- partial
evalShape :: Expr -> ExprF Int
evalShape (Fx (Const n)) = Const n
evalShape (Fx (Add l r)) = Add (alg $ evalShape l) (alg $ evalShape r)
evalShape (Fx (Mul l r)) = Mul (alg $ evalShape l) (alg $ evalShape r)

evalFull2 :: Expr -> Int
evalFull2 = alg . evalShape

-- eval using alg & fmap
evalFull3 :: Expr -> Int
evalFull3 = alg . evalShape'
  where
    evalShape' :: Expr -> ExprF Int
    evalShape' = fmap (alg . evalShape') . unFix

-- general evaluation function for any reduction / evaluator function
evalGeneral :: (ExprF a -> a) -> Expr -> a
evalGeneral red = red . evalShape'
  where
    evalShape' = fmap (red . evalShape') . unFix

----------------
-- F-Algebra

-- 1. structure: functor f (endofunctor)
-- 2. carrier type:  a
-- 3. evaluator: morphism from f(a) -> a
type Algebra f a = f a -> a

type SimpleA = Algebra ExprF Int
alg' :: SimpleA
alg' = alg

-- initial algebra (doesn't loose any information!)
type ExprInitAlg = Algebra ExprF (Fix ExprF)
ex_init_alg :: ExprInitAlg
ex_init_alg = Fx

-- homomorphism: a mapping that preserves certain strucutre, in case of an
-- algebra, we want to keep the functor fixed, so homomorphism should map
-- carrier types and evaluators.
--
-- homomorphism from initial algebra
-- g :: Fix f -> a
-- fmap g :: f (Fix f) -> f a

-- f (Fix f) --- fmap g ---> f a
--    |                       |
--   Fx                      alg
--    |                       |
--    v                       v
--  Fix f ------- g --------> a
--
--  Reversing Fx we get:
--
-- f (Fix f) --- fmap g ---> f a
--    ^                       |
--    |                       |
--  unFix                    alg
--    |                       |
--    |                       v
--  Fix f ------- g --------> a
--
--  So to implement g, our general evaluator, we should apply `unFix`, `fmap g`
--  and then `alg`.
--
-- g = alg . (fmap g) . unFix
--   where alg :: f a -> a

g :: Expr -> Int
g = alg . (fmap g) . unFix

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata algg = gg'
  where gg' = algg . (fmap gg') . unFix

-- Lists
data ListF a b = Nil | Cons a b

instance Functor (ListF a) where
  fmap f (Nil     ) = Nil
  fmap f (Cons a b) = Cons a (f b)

algSum :: ListF Int Int -> Int
algSum (Nil     ) = 0
algSum (Cons a b) = a + b

lst :: Fix (ListF Int)
lst = Fx $ Cons 1 $ Fx $ Cons 2 $ Fx $ Cons 3 $ Fx Nil

sumLst :: Fix (ListF Int) -> Int
sumLst = cata algSum

