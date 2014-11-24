{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall -Werror -fwarn-incomplete-patterns #-}
module EmptyCase where

data DEq a b where
  Refl :: DEq a a

data NoEmpty = T | F

-- shouldn't ever be able to call, but using error is non-ideal as doesn't
-- convey this directly and disabled any error checking GHC can do for the
-- function.
absurd' :: DEq True False -> a
absurd' _ = error "absurd"

-- GHC can check this, ensure that in fact there are no cases to match on and
-- we have complete case coverage as a value of `DEq True False` can never be
-- generated.
absurd :: DEq True False -> a
absurd x = case x of {}

-- ideally this should warn but incomplete-patterns doesn't flag yet.
valid :: NoEmpty -> NoEmpty
valid x = case x of {}

