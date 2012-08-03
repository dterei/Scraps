{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}
module FunDeps where

------------------------------------------------------------------------------
{- TEST 1: MultiParamTypeClass but NO functional dependencies -}

class TCa a b where
    -- GOOD:
    tca1 :: a -> b -> Int

    -- BAD: it compiles but I can't find any way to actually use tc3, all
    -- tries result in 'b' being ambiguous.
    tca3 :: a -> Int

    -- BAD:
    -- tca2 :: Int

-- BAD:
-- tca3yes :: forall a b. TCa a b => a -> b -> Int
-- tca3yes a b = tca1 a b + tca3 a

instance TCa Int Char where
    tca1 _ _ = 1
    tca3 _   = 2

instance TCa Int Int where
    tca1 _ _ = 3
    tca3 _   = 4

tcaTest :: Int
tcaTest = x + y + z
    where
        x = tca1 (1 :: Int) 'c'
        y = tca1 (1 :: Int) (2 :: Int)
        -- BAD:
        -- z = tca3 (1 :: Int)
        -- z = tca3yes (1 :: Int) 'c'
        z = 0

------------------------------------------------------------------------------
{- TEST 2: MultiParamTypeClass WITH functional dependencies -}

class TCb a b | a -> b where
    -- GOOD: 
    tcb1 :: a -> b -> Int
    -- GOOD: this now works due to fun deps
    tcb2 :: a -> Int
    -- BAD: won't compile, now way to determine dictionary
    -- tcb3 :: Int

    -- BAD: compiles, but no way to use it
    tcb4 :: b -> Int

instance TCb Int Char where
    tcb1 _ _ = 1
    tcb2 _   = 2
    tcb4 _   = 3

-- BAD: 'a uniquely determines b', so we can't have Int map to both Char and
-- Int, OR in other words, a must be unique as b is now ignored.
-- instance TCb Int Int where
--     tcb1 _ _ = 3
--     tcb2 _   = 4

instance TCb Char Char where
    tcb1 _ _ = 4
    tcb2 _   = 5
    tcb4 _   = 6

tcbTest :: Int
tcbTest = w + x + y + z
    where
        w = tcb1 (1 :: Int) 'c'
        -- BAD: anything with a instantiated to Int must have b instantiated to
        -- Char as that is the only valid instance for a as Int.
        -- y = tcb1 (1 :: Int) (2 :: Int)
        x = tcb1 'c' 'c'
        -- GOOD: These now work due to fun deps
        y = tcb2 (1 :: Int)
        z = tcb2 'c'
        -- BAD: No way to resolve amibuity of 'a' type variable
        -- k = tcb4 'c'

------------------------------------------------------------------------------
{- TEST 3: MultiParamTypeClass WITH functional dependencies AND a bidirectional
           relation-}

class TCc a b | a -> b, b -> a where
    -- GOOD: 
    tcc1 :: a -> b -> Int
    -- GOOD: this now works due to fun deps
    tcc2 :: a -> Int
    -- BAD: won't compile, now way to determine dictionary
    -- tcb3 :: Int

    -- GOOD: this now works due to fun deps
    tcc4 :: b -> Int 

instance TCc Int Char where
    tcc1 _ _ = 1
    tcc2 _   = 2
    tcc4 _   = 3

-- BAD: 'a uniquely determines b', so we can't have Int map to both Char and
-- Int, OR in other words, a must be unique as b is now ignored.
-- instance TCb Int Int where
--     tcb1 _ _ = 3
--     tcb2 _   = 4

-- BAD: As for above as now 'a uniquely determins b' AND 'b uniquely determines
-- a'. So thinking of b as ignored isn't really true, it is like said above. OR
-- perhaps think this, if we have 'x -> y' then x is now given the constraint
-- that all instances must have unique instantiations of x. e.g without we have
-- UNQIUE (x,y), with fun deps with have UNIQUE (x), with 'x -> y, y -> x' we
-- have UNIQUE (x) && UNIQUE (y).
--
-- instance TCc Char Char where
--     tcc1 _ _ = 4
--     tcc2 _   = 5
--     tcc4 _   = 6
instance TCc Bool Int where
    tcc1 _ _ = 4
    tcc2 _   = 5
    tcc4 _   = 6

tccTest :: Int
tccTest = w + x + y + z + k + l
    where
        w = tcc1 (1 :: Int) 'c'
        -- BAD: anything with a instantiated to Int must have b instantiated to
        -- Char as that is the only valid instance for a as Int.
        -- y = tcb1 (1 :: Int) (2 :: Int)
        x = tcc1 True (1 :: Int)
        -- GOOD: These now work due to fun deps
        y = tcc2 (1 :: Int)
        z = tcc2 True
        -- GOOD: These now work due to fun deps
        k = tcc4 'c'
        l = tcc4 (1 :: Int)

------------------------------------------------------------------------------
{- TEST 4: Subclass of MPTC ... -}

class TCa a b => TCd1 a b where
    tcd12 :: a -> b -> Int

class TCa a b => TCd2 a b | a -> b where
    tcd21 :: a -> b -> Int
    tcd22 :: a -> Int

instance TCd2 Int Char where
    tcd21 a b = tca1 a b
    -- BAD: Even this won't work...
    -- tcd22 a   = tca3 a
    tcd22 _ = 0

class TCb a b => TCd3 a b where
   -- do the fun deps carry through from TCb?
   tcd31 :: a -> b -> Int
   tcd32 :: a -> Int

instance TCd3 Int Char where
    tcd31 = tcb1
    -- yes they seem to...
    tcd32 = tcb2

-- BAD: due to fun dep constraint of superclass TCb...
-- instance TCd3 Int Int where
--     tcd31 = 0
--     tcd32 = 1

-- GOOD! needed ConstraintKinds + UndecidableInstances for this to work.
class (forall b. TCb a b) => TCd4 a where
    tcd41 :: TCb a b => a -> Int

-- BAD: Complains about not being able to deduce '(forall b. TCb Int b)'
-- instance TCd4 Int where
--     tcd41 = tcb2

-- BAD: Compiles but seems to be unusuable
instance (forall b. TCb Int b) => TCd4 Int where
    tcd41 = tcb2


-- BAD: Won't compiles, complians of not being able to deduce '(forall b. TCb
-- Int b)'...
-- tcd4Test :: Int
-- tcd4Test = tcd41 (1 :: Int)

------------------------------------------------------------------------------
{- TEST 6: From GHC Userguide -}

class HasConverter a b | a -> b where
    convert :: a -> b

data Foo a = MkFoo a

instance (HasConverter a b, Show b) => Show (Foo a) where
    show (MkFoo value) = show (convert value)

instance HasConverter Char Int where
    convert _ = 0

converterTest :: String
converterTest = show $ MkFoo 'c'

