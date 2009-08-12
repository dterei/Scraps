-- data type
-- (Name a b) is type
-- (Name String String) is concrete type constructor
-- (Full a b) is value constructor (e.g Full String Strin)
data Name a b = Full a b

-- type just renames, like C typedef. Need to use String constructor to get
-- A type. A and String are basically synonyms, a function which accepts
-- A also accepts String and visa-versa.
type A = String

-- Same as above but defining hard type for Name a b
type B = Name String String

-- newtype is half between data and type. It defines a constructor and must
-- be explicitly constructed. A function which accepts type B doesn't accept
-- type C and visa-versa. However, unlike data, it doesn't wrap around its
-- values (e.g B), so has lower overhead. The actual construction is removed
-- at compile time, treated more like a tuple at run time.
newtype C = C B

main = putStrLn "Hello!"

