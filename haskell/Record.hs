module Record where

data Record
  = A {
      field1 :: String,
      field2 :: String,
      field3 :: Bool
  }
  | B {
      field4 :: String,
      field2 :: String,
      field3 :: Bool
  }
