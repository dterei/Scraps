{-# LANGUAGE InstanceSigs #-}
-- Also see FAlgebra.hs for an alternative exploration
-- http://programmers.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern
-- http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html
-- https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html
-- http://degoes.net/articles/modern-fp

-- | Free - An exploration of free monads. Think of this as a light-weight way
-- to create a DSL. We pull our code into a data type and use a 'Free' monad to
-- provide the supporting infrastructure to make it easier to work with. Once
-- done, we can define different interpreters to operate over our DSL as we
-- like. One that adds logging, one that runs against the production database,
-- one that mocks the database.
module Free where

-- | First DSL attempt. Non-composable!
data DSL1
  = Get1 String
  | Set1 String String

-- | Second DSL: add in 'next' type parameter for composing operations.
data DSL2 next
  = Get String (String -> next)
  | Set String String next
  | End -- terminator / nil

-- | Run a get and then set the value against a new key.
getset1 :: DSL2 (DSL2 (DSL2 next))
getset1 = Get "key1" $ \val -> Set "key2" val End

-- Two things:
-- (1) Notice the 'next' parameter makes DSL2 a Functor! And with only one
-- possible instance.
-- (2) This DSL2 type is annoying to work with though as composition of
-- operators is reflected into the type. We'd like to construct a list at the
-- type level so that the value level list doesn't create an ever growing type.

-- Fixing (1)
instance Functor DSL2 where
  fmap f (Get name k)        = Get name (f . k)
  fmap f (Set name val next) = Set name val (f next)
  fmap f End                 = End

-- Fixing (2): This is where a 'free monad' comes in - giving us a type level
-- list.
data Free f a
  = Free (f (Free f a))
  | Return a

-- We can now give this type to getset - no growth!
getset2 :: Free DSL2 next
getset2 = Free (Get "key1" $ \val -> Free (Set "key2" val (Free End)))

-- Simple value
example1 :: Free DSL2 ()
example1 = Return ()

example2 :: Free DSL2 next
example2 = Free End

-- lets make example2 explicit in how the types are instantiated
example3 :: Free DSL2 next
example3 = Free x
  -- We instantiate DSL2 next as:
  where x = End :: DSL2 (Free DSL2 next)

example4 :: Free DSL2 next
example4 = Free s
  where s = Set "key" "val" (Free e) :: DSL2 (Free DSL2 next)
        e = End                      :: DSL2 (Free DSL2 next)

-- One problem stil - the above is very annoying to work with! So let's
-- construct a monad to avoid the boilerplate.
instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Free a)   = Free $ fmap (fmap f) a
  fmap f (Return n) = Return $ f n

instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Return

  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (<*>) (Free f)   a = Free $ fmap (<*> a) f
  (<*>) (Return f) a = fmap f a

instance Functor f => Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) (Free a)   f = Free $ fmap (>>= f) a
  (>>=) (Return a) f = f a

  return :: a -> Free f a
  return = pure

-- nicer again using monad
getset3 :: Free DSL2 next
getset3 = do
  key1 <- Free $ Get "key1" Return
  Free $ Set "key2" key1 (Return ())
  Free End

-- unpacking: nicer again using monad
getset3' :: Free DSL2 next
getset3' = 
  Free (Get "key1" Return) >>= \key1 -> 
    Free (Set "key2" key1 (Return ())) >>
      Free End

example5 :: Free DSL2 next
example5 = Free (Set "key2" "val" (Return ())) >> Free End

-- (>>) a b = a >>= \_ -> b
-- Free $ fmap (>>= \_ -> (Free End)) (Set "key2" "val" (Return ()))

-- Fixing (3): Free and Return are still ugly, luckily the way we 'lift' a
-- action into Free is always the same, so can exploit.
liftFree :: Functor f => f a -> Free f a
liftFree f = Free $ Return <$> f

-- Using (3) we can write nicer versions of our DSL:
get key     = liftFree $ Get key id
set key val = liftFree $ Set key val ()
end         = liftFree $ End

getset4 :: Free DSL2 a
getset4 = do
  key1 <- get "key1"
  set "key2" key1
  end

