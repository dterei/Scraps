{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implement standard type classes for a list type.
module MyList where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
-- import Data.Semigroup

-- | Our list type
data L a = Nil | Cons a (L a) deriving (Show, Read, Eq, Ord)

xs :: L Int
xs = Cons 4 $ Cons 3 $ Cons 2 $ Cons 1 Nil

-- Semigroup > Monoid
--
-- Foldable
--         \
--          > Traversable
--         /
-- Functor > Applicative > Monad > MonadFix
--                 \             \
--                  > Alternative > MonadPlus
--

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup (L a) where
  (<>) xs ys = xs `app` ys
    where app Nil         yy = yy
          app (Cons x xx) yy = Cons x (app xx yy)

instance Monoid (L a) where
  mempty = Nil
  mappend = (<>)

instance Foldable L where
  foldr :: (a -> b -> b) -> b -> L a -> b
  foldr f b xs = fr b xs
    where fr c Nil         = c
          fr c (Cons x xs) = f x (fr c xs)

instance Traversable L where
  traverse :: forall f a b. Applicative f => (a -> f b) -> L a -> f (L b)
  traverse f xs = foldr cons_f (pure Nil) xs
    where cons_f :: a -> f (L b) -> f (L b)
          cons_f y ys = fmap Cons (f y) <*> ys

  -- traverse :: forall f a b. Applicative f => (a -> f b) -> L a -> f (L b)
  -- traverse f xs = foldr (<*>) (pure Nil) xs_app
  --   where xs_app :: L (f (L b -> L b))
  --         xs_app = fmap (fmap Cons . f) xs

instance Functor L where
  -- even though Functor is a super class of Applicative, we can rely on
  -- applicative to implement fmap (although it's a little contrived as
  -- probably easier to implement fmap and use that to implement <*>)
  fmap = liftA

instance Applicative L where
  pure a = Cons a Nil
  fs <*> xs = foldMap (\f -> mmap f xs) fs
    where mmap f ys = foldr (\z zs -> Cons (f z) zs) Nil ys

instance Alternative L where
  empty = Nil
  (<|>) = mappend

instance Monad L where
  -- Since L is both a Monoid and Foldable, we get the Monad for free
  (>>=) = flip foldMap

instance MonadFix L where
  mfix :: (a -> L a) -> L a
  mfix f = case fix (f . lhead) of
              Nil        -> Nil
              (Cons x _) -> Cons x $ mfix (ltail . f)
    where lhead (Cons x _) = x
          lhead Nil        = error "lhead: empty L"
          ltail (Cons x xs) = xs
          ltail Nil         = error "ltail: empty L"

instance MonadPlus L
  -- mzero = empty
  -- mplus = <|>

-- | Example of how to use Traversable
data Sum a = S Int a deriving (Show, Read, Eq, Ord)

instance Functor Sum where
  fmap f (S n a) = S n (f a)

instance Applicative Sum where
  pure a  = S 0 a
  (S n f) <*> (S m a) = S (n+m) (f a)

toS :: Int -> Sum Int
toS n = S n n

sumL :: L Int -> Sum (L Int)
sumL = traverse toS

