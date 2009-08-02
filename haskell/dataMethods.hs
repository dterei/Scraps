
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

doAdd :: KindofInterface -> Int -> Int-> Int
doAdd a = simpleAdd a

main = do
	putStrLn "Hello World"
	putStrLn $ show $ doAdd implem 1 2
	putStrLn $ show $ doAdd revImp 1 2

