# loop-dsl

A simple loop dsl for monadic actions.

### Features

- [X] Index based looping.
- [X] For-each style iterates over a traversable.
- [X] Breaking out of loop with `quit` and `cease`.
- [X] While loop with `while` clause.
- [X] For-each while enumerating index.
- [X] Nested loop.
- [ ] Full type inference.

### PS:
- Loop breaking is achieved by stacking an ExceptT on top of the current monad, so the original action needs to be lifted.
- Some types can't be inferred at this moment, so you need to feed the type of elements of the container and the parameter. Or you can use the monormophized version in `Control.Monad.Loop.Internal`

```haskell
main :: MonadIO m => m ()
main = do

  -- loop over range.
  for [(0 :: Int)..] `with` \(i :: Int) -> do
    if i == 3 then quit else lift $ do
      putStr "loop 1"
      putStrLn $ "=> " ++ show i

  -- enumerating index.
  for [0..] `with` \(idx, val) -> do
    if idx == 3 then quit else $ do
      putStr "loop 2"
      putStrLn $ "=> idx: " ++ show idx ++ ", val: " ++ show val

  -- while loop
  for [(0 :: Int)..] `while` (<3) `with` \(val :: Int) -> do
    lift $ putStrLn "loop3"

  -- nested loop
  for [(0 :: Int)..] `while` (<3) `with` \(i :: Int) -> lift $ do
    for [(0 :: Int)..] `while` (<3) `with` \(j :: Int) -> lift $ do
      putStrLn $ show i ++ ", " ++ show j

  -- using `quit` and nested loops. quit is just throwError ().
  for [(0 :: Int)..] `while` (<3) `with` \(i :: Int) -> do
    if i == 2 then
      quit
    else lift $
      for [(0 :: Int)..] `while` (<3) `with` \(j :: Int) -> lift $ do
        putStrLn $ show i ++ ", " ++ show j

  -- break to the outer most loop with `cease`
  for [(0 :: Int)..3] `with` \(i :: Int) -> lift $ do
    for [(0 :: Int)..3] `with` \(j :: Int) -> do
      if j == 2 then cease else
        lift $ putStrLn $ show i ++ " " ++ show j
```


### nQeen with loop
```haskell
nQueen :: Int -> IO ()
nQueen n = do
  board <- newBoard
  nSols <- newMVar 0
  nqueen nSols board 0
  putStrLn . show =<< takeMVar nSols
  where
    newBoard = UV.replicate n (0 :: Int)
    nqueen nSols board i = flip runContT return . callCC $ \ret -> do
      boardConst <- G.freeze board
      let nestedSearch () = lift $ do
            for [(0::Int)..GM.length board-1] `with` \col -> liftIO $ do
              GM.unsafeWrite board i col
              nqueen nSols board (i + 1)

      when (i >= GM.length board) $ do
        when (good boardConst i) $ liftIO $ do
          nSols `modifyMVar_` (return . (+1))
          printBoard board
        ret ()

      when (not (good boardConst i)) $ ret ()
      nestedSearch ()

    threaten rA cA rB cB = rA == rB || cA == cB || abs (rA - rB) == abs (cA - cB)
    good board endIdx = flip evalState True $ do
      for [(0::Int)..endIdx-1] `with` \(rA :: Int) -> do
        for [rA+1..endIdx-1] `with` \(rB :: Int) -> lift $ do
          let cA = board G.! rA
          let cB = board G.! rB
          when (threaten rA cA rB cB) $ do
            lift $ put False
            cease
      get

    printBoard board = let loop = (for [(0::Int)..n-1] `with`) in do
      loop $ \(i::Int) -> liftIO $ do
        loop $ \(j::Int) -> liftIO $ do
          v <- board `GM.unsafeRead` i;
          putStr $ (if v == j then 'X' else '_') : " "
        putStrLn ""
      putStrLn ""
```

### Caveat

Under the hoode the loop body is in an `ExceptT () m ()`, so for a nested loop the inner loop body has type `ExceptT () (ExceptT m ()) ()`, which type checks even if you don't lift the action, which might leads to unintented behavior. For example, the following code is not really a double for loop:

```haskell
wrong = do
  for [(0::Int)..2] `with` \(i :: Int) -> do
    for [i+1..4] `with` \(j :: Int) -> do
      liftIO $ putStrLn $ show (i, j)
      if (rB == 3)
        then do liftIO $ putStrLn "cease!" >> cease
        else return ()

-- output:
-- (0,1)
-- (0,2)
-- (0,3)
-- cease!
-- (1,2)
-- (1,3)
-- cease!
-- (2,3)
-- cease!
```

Instead we need to lift the inner loop body to the first except

```haskell
right = do
  for [(0::Int)..2] `with` \(i :: Int) -> do
    for [i+1..4] `with` \(j :: Int) -> lift $ do
      liftIO $ putStrLn $ show (i, j)
      if (rB == 3)
        then liftIO $ putStrLn "cease!" >> cease
        else return ()
-- output:
-- (0,1)
-- (0,2)
-- (0,3)
-- cease!
```
