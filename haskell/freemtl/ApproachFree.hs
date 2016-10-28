-- http://degoes.net/articles/modern-fp

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Main where

-- Design Goals:
-- 1. High-level semantics (i.e., explicit)
-- 2. Built from as few, orthogonal operations as possible
-- 3. Rely on composition to satify other use cases

-- To Implement
-- ------------

data Free :: (* -> *) -> * -> *
data Coproduct :: (* -> *) -> (* -> *) -> * -> *
liftF = undefined
toLeft = undefined
toRight = undefined
instance Functor (Free f) where
  fmap = undefined
instance Applicative (Free (Coproduct f g)) where
  pure = undefined
  (<*>) = undefined


-- Placeholder Types
-- -----------------
type Path = String
type Bytes = String
type List a = [a]
data Unit = Unit


-- Cloud API for file storage
-- --------------------------
-- We can define the semantics of our API, but not yet how to actually provide
-- the service.
data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path (List Path -> a)

type CloudFilesAPI a = Free CloudFilesF a

saveFile :: Path -> Bytes -> CloudFilesAPI Unit
saveFile p b = liftF $ SaveFile p b Unit

listFiles :: Path -> CloudFilesAPI (List Path)
listFiles p = liftF $ ListFiles p id


-- REST API
-- --------
-- We can define the Cloud API in terms of another DSL: one for REST APIs.
data HttpF a
  = GET  Path (Bytes -> a)
  | PUT  Path Bytes (Bytes -> a)
  | POST Path Bytes (Bytes -> a)
  | DEL  Path (Bytes -> a)

-- Transform Cloud DSL into REST DSL:
cloudFilesI :: CloudFilesF a -> Free HttpF a
cloudFilesI = undefined


-- LOG API
-- -------
data Level = Debug | Info | Warn | Error
data LogF a = Log Level String a

-- We can map CloudFiles operations into the logging DSL too. This does
-- restrict our logging to a more fixed/context-free interpretation however.
logCloudFilesI :: CloudFilesF a -> Free LogF Unit
logCloudFilesI (SaveFile p _ _) = liftF $ Log Debug ("Save file to " ++ p) Unit
logCloudFilesI (ListFiles p _)  = liftF $ Log Debug ("Listing files at " ++ p) Unit



-- Composing Interpreters
-- ----------------------
logRestCloudFilesI :: CloudFilesF a -> Free (Coproduct LogF HttpF) a
logRestCloudFilesI op = toLeft (logCloudFilesI op) *> toRight (cloudFilesI op)


-- Final Interpreter
-- -----------------

executor :: Coproduct LogF HttpF a -> IO a
executor = undefined


-- Claimed Beneftits
-- -----------------
--
-- 1. Seperated API from implementation---code interacting with cloud API
-- doesn't care how it's implemented.
-- 2. Implementation of cloud API is completely isolated from rest of
-- application. This really shines when we switch between implementations,
-- e.g., have a seperate one for mocking/testing.
-- 3. Logging code is centralized and isolated, untagnled from other domains of
-- concern. Can also swich logging implementations to log to different sinks.
-- 4. Modular and composable, can even choose how we interpret the program
-- based on runtime values!


-- Main
-- ----
main :: IO ()
main = return ()
