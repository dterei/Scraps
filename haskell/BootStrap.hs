module BootStrap where

data B a = Two (B (a,a)) | One a
    deriving Show

