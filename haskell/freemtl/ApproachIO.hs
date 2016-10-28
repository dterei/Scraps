module Main where

import Control.Exception.Base
import Prelude hiding (log)

-- Helpers
-- -------
data Path = Path [String]
instance Show Path where { show (Path xs) = show xs }
name :: Path -> Path
name (Path xs) = Path [last xs]

parentDir :: Path -> Path
parentDir (Path xs) = Path $ init xs


-- LOG API
-- -------
log :: String -> IO ()
log = putStrLn


-- REST API
-- --------
type URL = String
data HttpResult = HttpResult

httpPost :: URL -> String -> IO HttpResult
httpPost _ _ = return HttpResult

httpOK :: HttpResult -> Bool
httpOK _ = True


-- Cloud File API
-- --------------
saveFile :: Path -> String -> IO ()
saveFile p f = do
  log $ "Saving file " ++ show (name p) ++ " to " ++ show (parentDir p)
  r <- httpPost ("cloudfiles.fooservice.com/" ++ show p) f
  if httpOK r
    then log $ "Successfully saved file " ++ show p
    else let msg = "Failed to save file " ++ show p
         in log msg *> throwIO (userError msg)


-- Exectutor
-- ---------
main :: IO ()
main = saveFile (Path ["home", "davidt", "work", "file.hs"]) ""
  
