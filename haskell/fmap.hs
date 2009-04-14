import System.Environment
import Text.Printf

meanB :: [Double] -> Double
meanB = go 0 0
	where
		go :: Double -> Int -> [Double] -> Double
		go s l []     = s / fromIntegral l
		go s l (x:xs) = go (s+x) (l+1) xs

meanR :: [Double] -> Double
meanR xs = meanR2 0 0 xs

meanR2 :: Double -> Int -> [Double] -> Double
meanR2 a b [] = a / fromIntegral b
meanR2 sum len (x:xs) =  meanR2 (sum + x) (len + 1) xs

ffmap = do
	[d] <- fmap (map read) getArgs
	printf "%f\n" (meanB [1 .. d])

main = ffmap

