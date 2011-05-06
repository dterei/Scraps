A O(n log(n)) algorithm is easy if you use Data.Map:

> import qualified Data.Map as Map
>
> splitStreamsMap :: Ord a => [(a,b)] -> Map.Map a [b]
> splitStreamsMap = foldl add Map.empty
>     where add m (a,b) = Map.insertWith (++) a [b] m
>
> splitStreams :: Ord a => [(a,b)] -> [(a,[b])]
> splitStreams = Map.toList . splitStreamsMap

Twan

DOESN'T WORK: Isn't lazy, Map is strict in that it must be
fully evaluated to call toList.
