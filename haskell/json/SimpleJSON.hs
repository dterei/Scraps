-- file: SimpleJSON.hs
-- Author: David Terei

data JValue = JString String
				| JNumber Double
				| JBool Bool
				| JNull
				| JObject [(String, JValue)]
				| JArray [JValue]
				deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

