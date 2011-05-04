{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import Data.Maybe

newtype MonadIO m a = MonadIO {returnIO :: m a}

update :: Monad m => MonadIO m a -> MonadIO m ()
update mio = mio {returnIO = return ()}

newtype Thingy m = Thingy {thing :: m ()}

update' :: Monad m => Thingy m -> Thingy m
update' t = t { thing = return () }

thingDef :: Monad m => Thingy m
thingDef = Thingy $ return ()

data Witness a where
    IntWitness  :: Witness Int
    BoolWitness :: Witness Bool
    CharWitness :: Witness Char
    NullWitness :: Witness a

class MyWitness a where
    getWitness :: Witness a

instance MyWitness Char  where getWitness = CharWitness
instance MyWitness Int   where getWitness = IntWitness
instance MyWitness Bool  where getWitness = BoolWitness
instance MyWitness a     where getWitness = NullWitness

dCast :: Witness a -> Witness b -> a -> Maybe b
dCast IntWitness  IntWitness  a = Just a
dCast BoolWitness BoolWitness a = Just a
dCast CharWitness CharWitness a = Just a
dCast _           _           _ = Nothing

addIntList :: MyWitness a => a -> [Int] -> [Int]
addIntList x xs = let b = dCast getWitness IntWitness x
                  in case b of
                         Just n  -> n:xs
                         Nothing -> xs

