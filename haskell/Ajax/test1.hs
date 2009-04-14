module Main where

import Data.Maybe (fromJust)

import Network.CGI
import Network.HTTP
import Network.URI
import Network.Stream
import Network.HTTP.Headers

import JSON


main :: IO()
main = runCGI cgiMain


cgiMain :: CGI CGIResult
cgiMain =
    do
        searchString <- getInput "ka"
        titleString <- getInput "kt"
        bodyString <- getInput "kb"
        startIndex <- getInput "s"
        length <- getInput "l"
        companies <- getInput "cp"
        categories <- getInput "c"
        order <- getInput "o"
        dateStart <- getInput "ds"
        dateEnd <- getInput "de"
        timeStart <- getInput "ts"
        timeEnd <- getInput "te"
        setHeader "Content-type" "text/plain"
        output("test")


dateFormat :: Maybe String -> Maybe String
dateFormat Nothing = Nothing
dateFormat x = Just (stringList(take 4 (fromJust(x))) ++ "-" ++ stringList(drop 4 (take 6 (fromJust(x)))) ++ "-" ++ stringList(drop 6 (fromJust(x))))


timeDate :: (Maybe String, Maybe String) -> Maybe String
timeDate (Nothing, Nothing) = Nothing
timeDate (a, Nothing) = Just ("[\"" ++ (fromJust a) ++ "\"]")
timeDate (Nothing, a) = Just ("[\"" ++ fromJust a ++ "\"]")
timeDate (a, b) = Just ("[\"" ++ fromJust a ++ "\", \"" ++ fromJust b ++ "\"]")


addJSONItem :: (Maybe String, String, Bool) -> String
addJSONItem (a, b, False) = "\"" ++ b ++ "\":" ++ fromJust a
addJSONItem (a, b, True) = "\"" ++ b ++ "\":" ++ "\"" ++ fromJust a ++ "\""


notNull :: (Maybe String, String, Bool) -> Bool
notNull (Nothing, _, _) = False
notNull (a, _, _) = True


buildJSON :: [(Maybe String, String, Bool)] -> String
buildJSON xs = "{\"search\":{" ++ flatten ([ addJSONItem x | x <- xs, notNull x ]) ++ "}}"


flatten :: [String] -> String
flatten xs = (concatMap (\x -> x ++ ",") (take (length xs - 1) xs)) ++ (xs !! (length xs - 1))


stringList :: [Char] -> String
stringList [] = ""
stringList a = a


-- HTTP Stuff

postHttp :: String -> String -> IO String
postHttp xuri body =
    case parseURI xuri of
        Nothing -> return "Invalid URI"
        Just uri -> do
            resp <- getData (Request uri POST (postHeaders body) body)
            return (either handleError handleGood resp)


postHeaders :: String -> [Header]
postHeaders body = [Header HdrContentLength ((show . length) body),
                    Header HdrContentType "text/plain;charset=utf-8"]


getData :: Request_String -> IO (Either ConnError Response_String)
getData req = do
    eresp <- simpleHTTP req
    return (eresp)


handleGood :: Response_String -> String
handleGood rs = rspBody rs


handleError :: ConnError -> String
handleError ce = show ce


