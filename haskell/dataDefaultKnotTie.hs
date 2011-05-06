import Control.Monad.Instances
import Data.Function

data Animal = Animal {
    catchPhrase :: String,
    speak       :: IO ()
}

protoAnimal :: Animal -> Animal
protoAnimal old = Animal {
    catchPhrase = catchPhrase old,
    speak = putStrLn $ catchPhrase old
}

-- fix f = let x = f x in x

dog2 = fix $ (\x -> protoAnimal $ x { catchPhrase = "Woof", speak = putStrLn "Raw!" } )

cat4 = fix $ (\x -> protoAnimal $ x { catchPhrase = "Meow" } )

cat3 = let c = protoAnimal (c { catchPhrase = "Meow" }) in c

dog3 = dog
    where dogInit = protoAnimal dog
          dog = dogInit { catchPhrase = "Woof", speak = putStrLn "Raw!" }

cat2 = cat
    where catInit = protoAnimal cat
          cat = catInit { catchPhrase = "Meow" }

cat = fix catInit
    where catInit = do
            self <- protoAnimal
            return self { catchPhrase = "Meow" }

dog = fix dogInit
    where dogInit = do
            self <- protoAnimal
            return self { catchPhrase = "Woof", speak = putStrLn "Rowwrlrw" }

