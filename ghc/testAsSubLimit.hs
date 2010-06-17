import System

header = "#include <stdio.h>\n\ntypedef struct info_table {\n\tconst int srt;\n\tconst int type;\n\tconst int args;\n} info_table;\n\nint empty_entry() {return 1;}"

table :: String -> Int -> String
table name id = "const info_table " ++ name ++ "_info_tb __attribute__ " ++
                "((section (\".text,\\\"ax\\\",@progbits\\n\\t.subsection "++ 
                (show id) ++ " #\"))) = {-1,1," ++ (show id) ++ "};"

function_dec :: String -> Int -> String
function_dec name id = "int " ++ name ++ "_entry() __attribute__ ((section" ++
                       "(\".text,\\\"ax\\\",@progbits\\n\\t.subsection " ++
                       (show id) ++ " #\")));"

function_def :: String -> String -> String
function_def prev name = "int " ++ name ++ "_entry()\n{\n\tint d = " ++ prev ++
                        "_entry();\n\tprintf(\"prev = %d\\n\", d);" ++
                        "\n\tint *p = (int*) (&" ++ name ++ "_entry) - 1;\n\treturn *p;\n}"

mkFunction :: String -> String -> Int -> String
mkFunction prev name id
  = let tbl = table name id
        fun = function_dec name (id + 1)
        imp = function_def prev name
    in fun ++ "\n" ++ imp ++ "\n\n" ++ tbl

genFunctions :: Int -> IO String
genFunctions n = genFuncs' [1..n] "empty"
    where
        genFuncs' [] prev = return prev
        genFuncs' xs prev = let x = 2 * (head xs)
                                n = "a" ++ (show x)
                            in do
                                putStrLn $ mkFunction prev n x
                                genFuncs' (tail xs) n

main = do
    n:_ <- getArgs
    putStrLn header
    last <- genFunctions (read n)
    putStrLn $ "int main()\n{\n\t" ++ last ++ "_entry();\n\treturn 1;\n}\n"
    return ()
