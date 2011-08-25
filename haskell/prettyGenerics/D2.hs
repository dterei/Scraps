{-# LANGUAGE GADTs, FlexibleInstances, TypeSynonymInstances #-}
{-
    GADT design.

    Use GADT to store many different types into one type,
    using a type class restriction to allow extraction to
    string or any other common operations needed.

    Problem: Using GADT's, fairly advance / new feature.
    Existential usage is a little ugly. Also inflexible as
    need all operators stored in the Txt class which we wrote,
    ideally we want users to be able to get their original types
    back for their render function.

    Doesn't really give me what I want.
-}
module D2 where

class Txt a where
    toStr :: a -> String

data DocBase where
    Str :: Txt a => a -> DocBase

instance Txt DocBase where
    toStr (Str a) = toStr a

data Doc
  = Empty
  | DocStr DocBase
  | Union Doc Doc

instance Txt Char where toStr c = [c]
instance Txt Int where toStr i = show i
instance Txt String where toStr = id

comma :: Doc
comma = DocStr (Str ',')

bracket :: Doc -> Doc
bracket d = Union (DocStr (Str '(')) (Union d (DocStr (Str ')')))

-- XXX: user supplied printer can only do operations on DocBase that
-- are defined in our type class! Although can make sure they are
-- efficient version of it as they supply the implementation of the
-- operations.
printDoc :: Doc -> (DocBase -> a -> a) -> a -> a
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

