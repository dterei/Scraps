A modified version of Twan's solution. His problem suffers from forcing the
evaluation of the whole Map to get the list out. We instead keep a list of
the inserted keys into the map and use lookup to retrieve them back.

A O(n log(n)) algorithm is easy if you use Data.Map:

> import qualified Data.Map as Map
> import Data.Maybe
> import Data.Set as Set
>
> splitStreamsMap :: Ord a => [(a,b)] -> Map.Map a [b]
> splitStreamsMap = foldl add Map.empty
>     where add m (a,b) = Map.insertWith (++) a [b] m
>
> splitStreams :: Show a => Ord a => [(a,b)] -> [(a,[b])]
> splitStreams ab = 
>     let xs = splitStreamsMap ab
>         go ([]) old = []
>         go ((a,_):abs) old =
>             let grab = case Map.lookup a xs of
>                            Just bs -> bs
>                            Nothing -> error $ "Can't find key: " ++ show a
>             in case Set.member a old of
>                    True  -> go abs old
>                    False -> (a, grab) : go abs (Set.insert a old)
>     in go ab Set.empty

Terei

DOESN'T WORK: Since while the a's are now lazy, the [b] list isn't still.
So fst $ head $ splitStreams inf works, but head $ snd $ head $ splitStreams inf
doesn't. Its only half lazy shall we say.

