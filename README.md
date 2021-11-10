# Loop

A loop dsl for monadic actions.

### Motivation

A simple looping dsl for monadic actions.

### Features

- [X] Index based looping.
- [X] For each style iterates over a traversable.
- [X] Breaking out of loop with `quit.`
- [X] While loop with `while` clause.
- [x] For each but enumerating index.
- [ ] Ergonomic nested loop.
- [ ] Full type infernece.

### PS:
- Breaking loop is achieved by stacking an ExceptT on top of the current monad, so the original actions needs to be lifted
- Some types can't be inferred at this moment, so you need to feed the type of elements of the container and the parameter. Or you can use the monormophized version in `Control.Monad.Loop.Internal`

```haskell
import Control.Monad.Loop
main :: MonadIO m => m ()
main = do

  -- loop over range.
  loop `across` [(0 :: Int)..] `with` \(i :: Int) -> do
    if i == 3 then quit else do
      lift $ putStr "loop 1"
      lift $ putStrLn $ "=> " ++ show i

  -- enumerating index.
  loop `across` [0..] `with` \(idx, val) -> do
    if idx == 3 then quit else do
      lift $ putStr "loop 2"
      lift $ putStrLn $ "=> idx: " ++ show idx ++ ", val: " ++ show val

  -- while loop
  loop `across` [(0 :: Int)..] `while` (<3) `with` \(val :: Int) -> do
    lift $ putStrLn "loop3"

  -- nested loop
  loop `across` [(0 :: Int)..] `while` (<3) `with` \(i :: Int) -> do
    lift $ loop `across` [(0 :: Int)..] `while` (<3) `with` \(j :: Int) -> do
      lift $ loop `across` [(0 :: Int)..] `while` (<3) `with` \(k :: Int) -> do
          lift $ putStrLn (show (i, j, k))
```
