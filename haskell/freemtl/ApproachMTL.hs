{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Taken from:
-- https://gist.githubusercontent.com/ocharles/6b1b9440b3513a5e225e/raw/9149e11cae62b19ec340e4796e59c948a20b7ca1/Modern%2520FP%2520with%2520mtl.hs
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (log)

--------------------------------------------------------------------------------
-- The API for cloud files.
class Monad m => MonadCloud m where
  saveFile :: Path -> Bytes -> m ()
  listFiles :: Path -> m [Path]

type Bytes = String
type Path = String

--------------------------------------------------------------------------------
-- The API for logging.
class Monad m => MonadLog m where
  log :: Level -> String -> m ()

data Level
  = Debug
  | Info

--------------------------------------------------------------------------------
-- The API for REST clients.
class Monad m => MonadRest m where
  get :: Path -> m Bytes
  put :: Path -> Bytes -> m Bytes

--------------------------------------------------------------------------------
-- An instrumenting implementation that adds logging to every call.
newtype CloudFilesLogT m a =
  CloudFilesLogT {runCloudFilesLogging :: m a}
  deriving (Functor,Applicative,Monad,MonadLog)

instance (MonadLog m,MonadCloud m) => MonadCloud (CloudFilesLogT m) where
  saveFile p bytes = do
    log Debug ("Saving file: " ++ p)
    lift (saveFile p bytes)
  listFiles path = do
    log Debug ("Listing " ++ path)
    lift (listFiles path)

instance MonadTrans CloudFilesLogT where
  lift = CloudFilesLogT

--------------------------------------------------------------------------------
-- An implementation of logging to standard out.
newtype StdoutLoggingT m a =
  StdoutLoggingT {runStdoutLoggingT :: m a}
  deriving (Functor,Applicative,Monad,MonadIO)

instance MonadIO m => MonadLog (StdoutLoggingT m) where
  log Info msg = liftIO (putStrLn ("[Info] " ++ msg))
  log Debug msg = liftIO (putStrLn ("[Debug] " ++ msg))

--------------------------------------------------------------------------------
-- An implementation of MonadCloud that uses a REST client.
newtype CloudFilesRestT m a =
  CloudFilesRestT {runCloudFilesRestT :: m a}
  deriving (Monad,Functor,Applicative,MonadRest,MonadLog)

instance MonadRest m => MonadCloud (CloudFilesRestT m) where
  saveFile path bytes = do
    put ("/file/" ++ path) bytes
    return ()
  listFiles path = do
    get ("/files/" ++ path)
    return ["MockFile"]

--------------------------------------------------------------------------------
-- A (non-functional) REST client.
newtype RestClientT m a =
  RestClientT {runRestClient :: m a}
  deriving (Monad,Functor,Applicative,MonadIO,MonadLog)

instance MonadIO m => MonadRest (RestClientT m) where
  get path = do
    liftIO (putStrLn $ "I should GET " ++ path)
    return []
  put path bytes = do
    liftIO (putStrLn $ "I should PUT " ++ path ++ " " ++ bytes)
    return []

--------------------------------------------------------------------------------
-- Our application only talks about MonadCloud and MonadLog.
app :: (MonadCloud m, MonadLog m) => m ()
app = do
  [f] <- listFiles "/home/ollie"
  log Info ("Found " ++ f)
  saveFile f "Ollie"
  return ()

-- Running the application chooses to instrument with extra logging, use the
-- REST client and to send all logs to stdout.
main :: IO ()
main =
  runStdoutLoggingT (runRestClient (runCloudFilesRestT (runCloudFilesLogging app)))
