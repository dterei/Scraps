# Exploring Effect Composition

Can we do better than `IO`? We want more fine-grained effects and easier ways
to test/mock. Or, to generalize further, to seperate the semantics of parts of
a program from its implementation.

## Blogs

http://www.slideshare.net/jdegoes/mtl-versus-free
http://degoes.net/articles/modern-fp-part-2

## Papers

http://www.cs.nott.ac.uk/~psztxa/publ/beast.pdf
http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
http://okmij.org/ftp/Haskell/extensible/more.pdf

## Motivating Example

Source: http://degoes.net/articles/modern-fp

Claim: The code below is ugly! FP isn't doing any better than C as we've just
relegated all effects to the IO monad.

```
saveFile :: Path -> Bytes -> IO Unit
saveFile p f = do
  log $ "Saving file" ++ show (name p) ++ " to " ++ show (parentDir p)
  r <- httpPost ("cloudfiles.fooservice.com/" ++ (show p)) f
  if httpOK r
    then log $ "Successfully saved file " ++ show p
    else let msg = "Failed to save file " ++ show p
         in log msg *> throwException (error msg)
```

Reasoning for claim:
1. It's hard to understand
2. It's hard to test
3. It conflates different concerns---logging, error handling, business
   logic---and untagling them will introduce more complexity
4. It mixes different levels of abstraction---REST API vs business logic
5. It distributes knowledge that should be centralized---such as where and how
   to interact with the cloud files API

