{-
    Type Class Design.

    Use type class to represent DocBase
    instead of a fixed data type. Type
    class only need to include the ability
    for us to turn Strings and Chars into
    DocBase.
-}
module D3 where

data DocBase a => Doc a
  = Empty
  | DocStr a
  | Union (Doc a) (Doc a)

class DocBase a where
  chr :: Char -> a
  str :: String -> a

comma :: DocBase a => Doc a
comma = DocStr (chr ',')

bracket :: DocBase a => Doc a -> Doc a
bracket d = Union (DocStr (chr '(')) (Union d (DocStr (chr ')')))

printDoc :: DocBase a => Doc a -> (a -> b -> b) -> b -> b
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

