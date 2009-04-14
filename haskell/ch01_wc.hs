-- file: ch01_wc.hs
-- Count lines in a file, aka wc

main = interact wordCount
	where wordCount input = "\t" ++
		show (length (lines input)) ++ "\t" ++
		show (length (words input)) ++ "\t" ++
		show (length input) ++ "\n"

