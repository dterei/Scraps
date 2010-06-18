module Main ( main) where

import IO
import Control.Monad (forM_, liftM, when)

main = do
    x <- sillyIO
	putStrLn $ "Testing Syntax Highlighting for Haskell: " ++ x
	return()

sillyIO = do
    return (x)
    where x = "hooray"::String

secondE []   = error "Don't give me nothing!"
secondE x:[] = []
secondE xs   = (head . tail)

somesyn
 = let var1 = 1::Int
        var2 = if var1 > 0 then Just 1 else Nothing
        var3 = case var2 of
                    Just n | n > 1 -> "Yes!"
                           | otherwise -> "No!"
                    Nothing -> "N/A!"
    in var3

safeOpenFile :: FilePath -> IOMode -> IO (Maybe Handle)
safeOpenFile file mode = do
	catch (Just `liftM` (openFile file mode)) (\e -> return Nothing)

getName :: String -> Maybe String
getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  liftM swapNames (lookup name db)

data KindofInterface = KindofInterface {
		simpleAdd :: Int -> Int -> Int,
		simpleSub :: Int -> Int -> Int
	}

implem :: KindofInterface
implem = KindofInterface {
	simpleAdd = \a b -> a + b,
	simpleSub = \a b -> a - b
}

type MyString = String
newtype YourString = SString String

div3s :: [Int]
div3s n = [x | x <- [1..n], x `mod` 3 == 0]

--. An illegal comment
--| An illegal comment
-- | A vaild comment
--A valid comment
plus1 :: (Num a) => a -> a
plus1 n = n + 1

{- A multiline comment!
    multi?  -}
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}
instance Show Sheep where
  show s = show (name s)
