{-
    Polymorphic / Minimal Design.

    Have no DocBase and just use plain
    polymorphism to store base Strings.
    Have to remove some core operations though
    as we have no way of constructing prepapared
    DocBase's.
-}
module D1 where

data Doc a
  = Empty
  | DocStr a
  | Union (Doc a) (Doc a)

-- CAN'T DO!
-- comma :: DocBase a => Doc a
-- comma = DocStr (Chr ',')

-- CAN'T DO!
-- bracket :: DocBase a => Doc a -> Doc a
-- bracket d = Union (DocStr (Chr '(')) (Union d (DocStr (Chr ')')))

printDoc :: Doc a -> (a -> b -> b) -> b -> b
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

