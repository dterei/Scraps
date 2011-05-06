Hello, 

I think it is possible to do it in O(n) (if you change the representation 
of the output stream). 

Note that the solution is quite similar toTwan van Laarhoven, 
and it should be trivial use "Map" instead of "Rel".

Here is my take on it:

The type of the output stream:

> type Rel a b = a -> [b]

Here are cons and nil:

> cons :: Eq a => (a,b) -> Rel a b -> Rel a b
> cons (x,y) l z
>    | x == z    = y : l x
>    | otherwise = l z

> nil :: Rel a b
> nil _ = []

and a lookUp function:

> lookUp :: Rel a b -> a -> [b]
> lookUp = id

Finally the splitStreams algorithm:

> splitStreams :: Eq a => [(a,b)] -> Rel a b
> splitStreams = foldr cons nil

Here is a test with finite lists:

> fin = splitStreams [(3,'x'),(1,'y'),(3,'z'),(2,'w')]

and here are the console tests:

*General7> lookUp fin 1
"y"
*General7> lookUp fin 2
"w"
*General7> lookUp fin 3
"xz"

Now with infinite lists:

> inf = splitStreams (map (\x -> (0, x)) [1..])

and here a case where it terminates with infinite lists:

*General7> take 10 (lookUp inf 0)
[1,2,3,4,5,6,7,8,9,10]

Cheers,

Bruno Oliveira

DOESN'T WORK: Well it does but cheats really by only computing
one element 'a' at a time.

