something like this, then?

> splitStreams :: Ord a => [(a, b)] -> [(a, [b])]
> splitStreams [] = []
> splitStreams ((a, b) : t) = (a, b : bs) : splitStreams t'
>     where
>     (bs, t') = foldr f ([], []) t
>     f z@(a', b') (bs, t') = if a' == a
>                             then (b' : bs, t')
>                             else (bs, z : t')

(i guess i should use a strictness '!' for (xs, ys), but i am still
running ghc-6.5, and i don't like the case constructions that much.
does this bite me here?  i didn't check, really.)

who can tune this any further?

cheers,
m.

DOESN'T WORK: Isn't lazy
