module Main where

main = do
    putStrLn sELF
    putStrLn $ "sELF = \"" ++ sELF ++ "\""

sELF = "module Main where\n\nmain = do\n\tputStrLn sELF\n\nputStrLn $ \"sELF = \\\"\" ++ sELF ++ \"\\\"\""

