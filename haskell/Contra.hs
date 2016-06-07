-- https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance
-- http://blog.ezyang.com/2012/01/modelling-io/

{-# LANGUAGE RankNTypes #-}
module Contra where

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
  bimap f1 f2 (a,b) = (f1 a, f2 b)

instance Bifunctor Either where
  bimap f1 _  (Left  a) = Left  (f1 a)
  bimap  _ f2 (Right b) = Right (f2 b)

instance Bifunctor (->) where
  -- bimap f1 f2 (a -> c) :: b -> d
  bimap f1 f2 ac = \b -> undefined

class Profunctor p where
  dimap :: (b -> a) -> (c -> d) -> p a c -> p b d

instance Profunctor (->) where
  -- dimap f1 f2 (a -> c) :: b -> d
  dimap f1 f2 ac = \b -> let a = f1 b
                         in f2 (ac a)

class Monad m => MonadMorphIO m where
  morphIO :: (forall b. (m a -> IO b) -> IO b) -> m a

instance MonadMorphIO IO where
  morphIO fm = fm id

