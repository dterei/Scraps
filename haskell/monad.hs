type Seed = Int
type Random a = Seed -> (a, Seed)



randomNext :: Seed -> Seed
randomNext rand = if newRand > 0 then newRand else newRand + 2147483647
    where newRand = 16807 * lo - 2836 * hi
          (hi,lo) = rand `divMod` 127773

toDieRoll :: Seed -> Int
toDieRoll seed = (seed `mod` 6) + 1

rollDie    :: Random Int
rollDie seed = ((seed `mod` 6) + 1, randomNext seed)

sumTwoDice :: Random Int
sumTwoDice = \seed0 ->
  let (die1, seed1) = rollDie seed0
      (die2, seed2) = rollDie seed1
  in (die1 + die2, seed2)

(>>>) :: Random a -> Random b -> Random b
(>>>) m n = \seed0 ->
  let (result1, seed1) = m seed0
      (result2, seed2) = n seed1
  in (result2, seed2)

