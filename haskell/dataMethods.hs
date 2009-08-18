-- Test of using data defenitions to create a kind of interface
data KindofInterface
	= KindofInterface {
		simpleAdd :: Int -> Int -> Int,
		simpleSub :: Int -> Int -> Int
	}

implem :: KindofInterface
implem = KindofInterface {
	simpleAdd = \a b -> a + b,
	simpleSub = \a b -> a - b
}

revImp :: KindofInterface
revImp = KindofInterface {
	simpleAdd = \a b -> a - b,
	simpleSub = \a b -> a + b
}

main = do
	putStrLn "Hello World"
	putStrLn $ show $ simpleAdd implem 1 2
	putStrLn $ show $ simpleAdd revImp 1 2

