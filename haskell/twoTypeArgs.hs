module Main where

newtype CmdLineP s a = CmdLineP { runCmdLine :: s -> (a, s) }

instance Monad (CmdLineP s) where
    return a = CmdLineP $ \s -> (a, s)
    m >>= k  = CmdLineP $ \s ->
                let (a, s') = runCmdLine m s
                in runCmdLine (k a) s'

dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags = []

data DynFlags = DynFlags {
    field1 :: String,
    field2 :: String,
    field3 :: Bool
  }

data Flag m = Flag {
    flagName    :: String,
    flagOptKind :: OptKind m
  }

data OptKind m
  = NoArg  (m ())
  | HasArg (String -> m ())

