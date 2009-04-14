-----------------------------------------------------------------------------
-- |
-- Module      :  JSON
-- Copyright   :  (c) Masahiro Sakai & Jun Mukai 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  sakai@tom.sfc.keio.ac.jp
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module JSON
    ( Value (..)
    , parse
    , json
    , stringify
    , stringify'
    , toDoc
    , toDoc'
    ) where

import Control.Monad hiding (join)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Text.PrettyPrint.HughesPJ hiding (char)
import Text.Printf (printf)
import Data.Char (ord, chr, isControl)
import Data.List (unfoldr)
import Data.Bits
import qualified Data.Map as M

-- ---------------------------------------------------------------------------
-- The Value data type

data Value
    = String String
    | Number !Double
    | Object !(M.Map String Value)
    | Array [Value]
    | Bool !Bool
    | Null
    deriving (Eq,Show)

{-
instance Show Value where
    showsPrec p = showsPrec p . toDoc
-}

-- ---------------------------------------------------------------------------
-- The JSON Parser

parse :: String -> Maybe Value
parse s = case P.parse json "JSON.parse" s of
          Left err -> Nothing
          Right v  -> Just v

json :: Parser Value
json = spaces >> tok value

tok :: Parser a -> Parser a
tok p = do{ x <- p; spaces; return x }

value :: Parser Value
value = msum
        [ liftM String str
        , liftM Number number
        , liftM Object object
        , liftM Array  array 
        , string "true"  >> return (Bool True)
        , string "false" >> return (Bool False)
        , string "null"  >> return Null
        ]

str :: Parser String
str = liftM decodeSurrogatePairs $
      between (char '"') (char '"') $ many c1
    where c1 =  satisfy (\c -> not (c=='"' || c=='\\' || isControl c))
            <|> (char '\\' >> c2)
          c2 = msum
               [ char '"'
               , char '\\'
               , char '/'
               , char 'b' >> return '\b'
               , char 'f' >> return '\f'
               , char 'n' >> return '\n'
               , char 'r' >> return '\r'
               , char 't' >> return '\t'
               , char 'u' >> do xs <- count 4 hexDigit
                                return $ read $ "'\\x"++xs++"'"
               ]

number :: Parser Double
number = liftM read $ int >>+ option "" frac >>+ option "" exp
    where digits = many1 digit
          int    = option "" (string "-") >>+ digits
          frac   = char '.' >>: digits
          exp    = e >>+ digits
          e      = oneOf "eE" >>: option "" (string "+" <|> string "-")
          (>>+)  = liftM2 (++)
          (>>:)  = liftM2 (:)

object :: Parser (M.Map String Value)
object = liftM M.fromList $
         between (tok (char '{')) (char '}') $
         tok member `sepBy` tok (char ',')
    where member = do k <- tok str
                      tok (char ':')
                      v <- value
                      return (k,v)

array :: Parser [Value]
array  = between (tok (char '[')) (char ']') $
         tok value `sepBy` tok (char ',')

decodeSurrogatePairs :: String -> String
decodeSurrogatePairs = unfoldr phi
    where
      phi :: String -> Maybe (Char, String)
      phi (h:l:xs)
          | '\xD800' <= h && h <= '\xDBFF' &&  '\xDC00' <= l && l <= '\xDFFF'
              = seq c $ Just (c, xs)
                where c  = chr $ ((ord h .&. 1023) `shiftL` 10 .|. ord l .&. 1023) + 0x10000
      phi (x:xs) = Just (x, xs)
      phi [] = Nothing

-- ---------------------------------------------------------------------------
-- The JSON Printer

stringify :: Value -> String
stringify = stringify' (const False) 

stringify' :: (Char -> Bool) -> Value -> String
stringify' needEscape = show . toDoc' needEscape

toDoc :: Value -> Doc
toDoc = toDoc' (const False)

toDoc' :: (Char -> Bool) -> Value -> Doc
toDoc' needEscape = go
    where
      go :: Value -> Doc
      go (String s) = strToDoc s
      go (Number x)
          | isInfinite x = error "can't stringify infinity"
          | isNaN x      = error "can't stringify NaN"
          | otherwise    = double x
      go (Object m) = lbrace <+> join comma members $+$ rbrace
          where members = [fsep [strToDoc k <> colon, nest 2 (go v)]
                           | (k,v) <- M.toList m]
      go (Array xs) = lbrack <+> join comma (map go xs) <+> rbrack
      go (Bool b)   = text $ if b then "true" else "false"
      go Null       = text "null"

      strToDoc :: String -> Doc
      strToDoc = doubleQuotes . text . concatMap f
          where f '"'  = "\\\""
                f '\\' = "\\\\"
                f '\b' = "\\b"
                f '\f' = "\\f"
                f '\n' = "\\n"
                f '\r' = "\\r"
                f '\t' = "\\t"
                f c | isControl c || needEscape c =
                        if c < '\x10000'
                        then printf "\\u%04x" c
                        else case makeSurrogatePair c of
                             (h,l) -> printf "\\u%04x\\u%04x" h l
                    | otherwise = [c]

join :: Doc -> [Doc] -> Doc
join s = fcat . punctuate s

makeSurrogatePair :: Char -> (Char,Char)
makeSurrogatePair c = (chr h, chr l)
    where c' = ord c
          h  = (c' - 0x10000) `shiftR` 10 .|. 0xd800
          l  = c' .&. 1023 .|. 0xdc00
