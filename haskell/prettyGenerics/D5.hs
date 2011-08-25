{-# LANGUAGE TypeFamilies #-}
{-
    Theoretical Type Family Design.

    Problem: I still need a type class,
    so while I find this in some ways more elegant
    with the type class and data tied together it
    doesn't buy me anything really at the cost
    of using a more advance and recent extension.
-}
module D5 where

class DocBase a where
    data Base a :: *
    chr :: Char -> Base a
    str :: String -> Base a

data Default

instance DocBase Default where
    data Base Default = Chr Char | Str String
    chr = Chr
    str = Str

data DocBase a => Doc a
  = Empty
  | DocStr (Base a)
  | Union (Doc a) (Doc a)

comma :: DocBase a => Doc a
comma = DocStr $ chr ','

bracket :: DocBase a => Doc a -> Doc a
bracket d = Union (DocStr (chr '(')) (Union d (DocStr (chr ')')))

printDoc :: DocBase a => Doc a -> (Base a -> b -> b) -> b -> b
printDoc Empty _               end = end
printDoc (DocStr b) printer    end = printer b end
printDoc (Union d1 d2) printer end = printDoc d1 printer
                                     (printDoc d2 printer end)

