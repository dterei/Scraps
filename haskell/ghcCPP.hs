{-# LANGUAGE CPP #-}
module Main where

main = putStrLn $ "Testing __GLASGOW_HASKELL__: " ++ ver

ver :: String
#if __GLASGOW_HASKELL__ == 704
ver = "704"
#elif __GLASGOW_HASKELL__ == 703
ver = "703"
#elif __GLASGOW_HASKELL__ == 702
ver = "702"
#elif __GLASGOW_HASKELL__ == 701
ver = "701"
#elif __GLASGOW_HASKELL__ == 700
ver = "700"
#elif __GLASGOW_HASKELL__ == 699
ver = "699"
#elif __GLASGOW_HASKELL__ == 698
ver = "698"
#elif __GLASGOW_HASKELL__ == 697
ver = "697"
#elif __GLASGOW_HASKELL__ == 680
ver = "680"
#elif __GLASGOW_HASKELL__ == 613
ver = "613"
#elif __GLASGOW_HASKELL__ == 612
ver = "612"
#elif __GLASGOW_HASKELL__ == 611
ver = "611"
#elif __GLASGOW_HASKELL__ == 610
ver = "610"
#elif __GLASGOW_HASKELL__ == 608
ver = "608"
#else
ver = "unknown"
#endif

