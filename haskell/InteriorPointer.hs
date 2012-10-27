module InteriorPointer where

data X = X {-# UNPACK #-} !Int
           {-# UNPACK #-} !Int
           {-# UNPACK #-} !String

x = X 1 2 "hello"

mkIp (X _ _ c) = c

ip = mkIp x

