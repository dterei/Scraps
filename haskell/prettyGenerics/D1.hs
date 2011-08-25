{- Current Design #-}
{-
    Fixed design.

    Problem: Inflexible. How to have multiple clients
    easily use? Efficiency when other String types
    need to be converted to String.
-}
module D1 where

data Doc
  = Empty
  | DocStr DocBase
  | Union Doc Doc

data DocBase
  = Str String
  | Chr Char

comma :: Doc
comma = DocStr (Chr ',')

bracket :: Doc -> Doc
bracket d = Union (DocStr (Chr '(')) (Union d (DocStr (Chr ')')))

printDoc :: Doc -> (DocBase -> a -> a) -> a -> a
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

