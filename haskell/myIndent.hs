module Main where

main
  = do  { putStrLn "Hello World"
	; putStrLn "Yes?"
		; input <- getLine
	; case input of
		"a" -> putStrLn "A"
		"b" -> putStrLn "B"
		_ ->  putStrLn "Other"

; putStrLn "Done"
  }

