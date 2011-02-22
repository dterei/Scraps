{-# LANGUAGE GADTs, TypeOperators #-}

data GList a where
    Nil  :: Num a => GList a
    Cons :: Num a => a -> GList a -> GList a
    
data Foo where MkFoo :: a -> (a -> Bool) -> Foo

data GObject where
    MkObj :: a -> GObject

data a :+: b = Left' !a | Right' !b

