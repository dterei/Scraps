infin :: Double -> Double -> Double
infin 0 v = v
infin n v = infin (n-1) (v + 1/n)

main =
	let x = infin 1000000000 0
	in
		putStrLn (show x)

