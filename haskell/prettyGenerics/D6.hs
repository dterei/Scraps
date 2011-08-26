{- Current Design #-}
{-
    Fixed design + Polymorphicj

    Here we add a polymorphic constructor
    to DocBase.
-}
module D6 where

data Doc a
  = Empty
  | DocStr (DocBase a)
  | Union (Doc a) (Doc a)

data DocBase a
  = Str String
  | Chr Char
  | Free a

comma :: Doc a
comma = DocStr (Chr ',')

bracket :: Doc a -> Doc a
bracket d = Union (DocStr (Chr '(')) (Union d (DocStr (Chr ')')))

printDoc :: (Doc a) -> (DocBase a -> b -> b) -> b -> b
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

