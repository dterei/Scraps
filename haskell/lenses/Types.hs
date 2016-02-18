{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

-- http://www.scs.stanford.edu/16wi-cs240h/slides/lenses.html
module Types where

-- # Counting Values
-- How many values can these types hold?
data Empty
data One = One
data Two = TA | TB
data A = A Bool | B Ordering

-- Product types
-- Can hold number of values equal to `a x b x c`
data Product a b c = Product a b c

-- Sum types
-- Can hold number of values equal to `a + b + c`
data Sum a b c = SA a | SB b | SC c

-- # Nest Data Types

data OutlierEffect
    = Unaffected
    | Slight
    | Moderate
    | Severe

data OutlierVariance = OutlierVariance {
      ovEffect      :: OutlierEffect
    , ovDescription :: String
    , ovFraction    :: Double
    }

data SampleAnalysis = SampleAnalysis {
      anMean       :: [Double]
    , anStdDev     :: [Double]
    , anOutlierVar :: OutlierVariance
    }

data Payload = Payload {
      sample         :: [Double]
    , sampleAnalysis :: SampleAnalysis
    }

-- accessing easy!
effect :: Payload -> OutlierEffect
effect = ovEffect . anOutlierVar . sampleAnalysis

-- updating sucks!
editEffect :: (OutlierEffect -> OutlierEffect)
           -> Payload -> Payload
editEffect eff payload =
    payload {
      sampleAnalysis = analysis {
        anOutlierVar = variance {
          ovEffect = eff effect
        }
      }
    }
  where analysis = sampleAnalysis payload
        variance = anOutlierVar analysis
        effect   = ovEffect variance

-- haskell record update syntax a non-composable hack!
-- create new copy of record, change just the one field when copyng.
setOVDescription :: String -> OutlierVariance -> OutlierVariance
setOVDescription desc ov = ov { ovDescription = desc }

-- # Our Desires
-- 1) Access fields in records
-- 2) Compose access so can inspect nested records
-- 3) Update fields in records
-- 4) Compose updates so can modify nested records

-- Haskell record syntax gives us (1), (2) and sort of (3), but defiantly not
-- (4).

-- # Focusing and Holes
-- Suppose we have a pair, let's refer to the idea of being interested in the
-- second element as *focusing* on it.
-- lets refer to the slot we want to fill when editing a type as a *hole*

editFst :: (a -> c) -> (a,b) -> (c,b)
editFst f (a,b) = (f a, b)

editSnd :: (b -> c) -> (a,b) -> (a,c)
editSnd f (a,b) = (a, f b)

-- # Counting Holes

-- If drop b from (a,b), how many values does the resulting type have?

-- If want to drop a field from (a,b,c) can represent this as:
data Hole3 a b c = AHole b c | BHole a c | CHole a b

-- If we substitute x for a, b, and c, then (a,b,c) has x^3 values.
-- Hole3 has 3x^2 values. Symbolic differentiation!

-- # Using Holes: Creating

-- hole type for pairs
data PairHole a b = HoleFst b | HoleSnd a

-- if pull a value out of the hole, need to store somewhere. We have extra
-- type-parameter c so we can choose a new type of value to store in the hole
-- later.
data PairZipper a b c = PZ c (PairHole a b)

-- Nice thing with these functions is the polymorphism forces one, and only one
-- implementation!
focusFst :: (a,b) -> PairZipper a b a
focusFst (a,b) = PZ a $ HoleFst b

focusSnd :: (a,b) -> PairZipper a b b
focusSnd (a,b) = PZ b $ HoleSnd a

unfocusFst :: PairZipper a b c -> (c,b)
unfocusFst (PZ a (HoleFst b)) = (a,b)

unfocusSnd :: PairZipper a b c -> (a,c)
unfocusSnd (PZ b (HoleSnd a)) = (a,b)

-- # Holes: Accessing focused value

view :: PairZipper a b c -> c
view (PZ c _) = c

-- # Holes: Editing focus value

over :: (c -> d) -> PairZipper a b c -> PairZipper a b d
over f (PZ c l) = PZ (f c) l


-- # Holes Problems
-- 1. Need to specify hole at start and end of pipline
-- 2. Can't compose focusFst and focusSnd to get a new zipper

-- # Holes 2
-- Lets manage focusFst and unfocusFst together automatically:

-- t: I'll give you back a 't' when done
-- a: I'm focusing on 'a'
-- b: I might change 'a' to 'b'
data Focused t a b = Focused
  { focused :: a         -- focused element
  , rebuild :: b -> t    -- rebuild original value
  }

-- Original value to a Focused:
-- * Give me an 's', I'll focus on 'a',
-- * and maybe change 'a' type to 'b' in the process,
-- * once done focusing, I might give you back a 't' ('s' type with 'a'
--   replaced by 'b')
type Focuser s t a b = s -> Focused t a b

unfocus' :: Focused s a a -> s
unfocus' (Focused a rb) = rb a

view' :: Focuser s t a b -> s -> a
view' f s = focused $ f s

over' :: Focuser s t a b -> (a -> b) -> s -> t
over' f mod s = let Focused fa rb = f s
                in rb $ mod fa

_1 :: Focuser (a,b) (c,b) a c
_1 (a,b) = Focused a (,b)

_2 :: Focuser (a,b) (a,c) b c
_2 (a,b) = Focused b (a,)

focusHead :: Focuser [a] [a] a a
focusHead []     = error "empty list!"
focusHead (x:xs) = Focused x (:xs)

-- how to unify
-- over :: Focuser s t a b -> (a -> b) -> s -> t
-- view :: Focuser s t a b ->          -> s -> a

wat :: Focuser s t a b -> (a -> f b) -> s -> f t
wat = undefined

-- f = id
-- wat  :: Focuser s t a b -> (a -> f b) -> s -> f t
-- over :: Focuser s t a b -> (a ->   b) -> s ->   t

-- f = const a
-- wat  :: Focuser s t a b -> (a -> f b) -> s -> f t
-- view :: Focuser s t a b {- ignored -} -> s -> a

-- Data.Functor.Indentity
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- Control.Applicative
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
  fmap _ (Const v) = Const v

-- # Lenses: Our final type!
--
-- * Give me an 's', and I will focus on its element of type a
-- * If you use 'over' to edit, you can change those 'a' to 'b'
-- * Once done editing, you'll get back a 't' (which if you didn't change 'a'
--   to 'b' will be an 's')
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lover :: Lens s t a b -> (a -> b) -> s -> t
lover l f s = runIdentity $ l (Identity . f) s

lview :: Lens s t a b -> s -> a
lview l s = getConst $ l Const s

l_1 :: Lens (a,b) (c,b) a c
l_1 f (a,b) = (,b) <$> f a

l_2 :: Lens (a,b) (a,c) b c
l_2 f (a,b) = (a,) <$> f b

l_head :: Lens [a] [a] a a
l_head f (a:as) = (:as) <$> f a

-- ( l_1 . l_head ) = \a -> l_1 (l_head a)
-- l_1    :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
-- l_head :: Functor f => (a -> f a) -> [a]    -> f [a]

-- l_head a             :: [a] -> f [a]
-- l_1 (l_head a)       :: ([a], b) -> f ([a], b)
-- \a -> l_1 (l_head a) :: (a -> f a) -> ([a], b) -> f ([a], b)
--
