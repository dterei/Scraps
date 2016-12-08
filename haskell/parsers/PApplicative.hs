module PApplicative where

newtype Parser a = P { runP :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = P $ \inp -> [ (f a, ss) | (a, ss) <- runP p inp ]

instance Applicative Parser where
  pure a = P $ \inp -> [(a, inp)]
  pf <*> pa = P $ \inp0 -> [ (f a, inp2) | (f, inp1) <- runP pf inp0,
                                           (a, inp2) <- runP pa inp1 ]

any :: Parser Char
any = P go
  where go [] = []
        go (x:xs) = [(x,xs)]

-- | I can't implement `sat` by composing `any` with the predicate as that
-- requires `bind` so that I can branch. Without `Parser` being a monad, all
-- branching decisions must be constructed by 'opening' up `Parser` and
-- implementing the operation as a primitive.
--
-- So in that sense, the `any` operation above is far less useful as it's often
-- used by composing with `bind`.
--
-- Now, the lack of a Monad instance for Parser doesn't mean we have no
-- branching at all. Indeed, `any` and `sat` do just that. But it means that
-- consumers of the library (who can't open up Parser), are limited to using
-- the branching operations provided, they can't extend Parser.
sat :: (Char -> Bool) -> Parser Char
sat pred = P go
  where go [] = []
        go (x:xs) | pred x = [(x,xs)]
        go (x:xs) = []

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = pure []
string (x:xs) = char x *> string xs *> pure (x:xs)

