import Debug.Trace

ismaxl :: (Ord a) => [a] -> a -> Bool
ismaxl l x = x == maxel
            where maxel = trace "Hello (greedy)" $ maximum l

ismaxl' :: (Ord a) => [a] -> a -> Bool
ismaxl' l = \x -> x == maxel
            where maxel = trace "Hello (shared)" $ maximum l

main = do
    let list = [1,3..300]
    -- Each call is O(n), maxel isn't shared
    let ismax = ismaxl list
    let non = map ismax [1,2,3,4,300]
    -- Each call is O(1), maxel isn't shared
    let ismax' = ismaxl' list
    let shared = map ismax' [1,2,3,4,300]
    putStrLn $ "Not Shared: " ++ show non
    putStrLn $ "Shared: " ++ show shared

