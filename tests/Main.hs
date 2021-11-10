
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Control.Monad.Loop
import           Control.Monad.Loop.Internal
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Writer
import           Data.IORef


main :: IO ()
main = do putStrLn "sad"


-- loop in simple monad.
loopTestIO :: IO ()
loopTestIO = do
  for [0..] `with_` \i -> do
    if i == 3 then quit else do
      lift $ do
        putStr "loop 1"
        putStrLn $ "=> " ++ show i

  for [0..] `withi_` \(idx, val) -> do
    if idx == 3 then quit else lift $ do
      putStr "loop 2"
      putStrLn $ "=> idx: " ++ show idx ++ ", value: " ++ show val

  for [0..] `while` (<3) `withWhile_` \val -> do
    lift $ putStrLn "loop3"

loopTestVM = runVM [ Push 1, Push 2, Add, Pop ] $ do
  for [0..5] `with_` \i -> lift $ do
    eval

loopTestWithClassIO :: IO ()
loopTestWithClassIO = do
  for [(0 :: Int)..3] `with` \(i :: Int, v :: Int) -> do
    lift $ putStrLn $ show i ++ show ", " ++ show v

  for [(0 :: Int)..3] `with` \(i :: Int) -> do
    lift $ putStrLn $ show i

  for [(0 :: Int)..] `while` (<3) `with` \(val :: Int)  -> do
    lift $ putStrLn "loop3"

  for [(0 :: Int)..] `while` (<3) `with` \(i :: Int) -> lift $ do
    for [(0 :: Int)..] `while` (<3) `with` \(j :: Int) -> lift $ do
      putStrLn $ show i ++ ", " ++ show j

loopTestMutation :: IO ()
loopTestMutation = do
  counter <- newIORef 0
  for [(0::Int)..] `while` (<3) `with` \(i::Int) -> lift $ do
    c <- readIORef counter
    putStrLn $ "old current counter is: " ++ show c
    counter `modifyIORef'` (+1)
    c <- readIORef counter
    putStrLn $ "new current counter is: " ++ show c

-- custom monad.
data Code = Push Int | Pop | Add deriving Show
type Input = [Code]; type Output = [Int]; type Stack = [Int]
newtype VM_ a = VM_ { unVM :: WriterT Output (ReaderT Input (State Stack)) a }
  deriving ( Functor, Applicative, Monad, MonadReader Input , MonadWriter Output , MonadState Stack)
type VM = VM_ ()
runVM input = flip evalStateT [] . flip runReaderT input . execWriterT . unVM

evalCode :: Code -> VM
evalCode (Push n) = modify (n:)
evalCode Pop = do xs <- get; case xs of []     -> return (); (a:as) -> do put as; tell [a];
evalCode Add = do xs <- get; case xs of (a:b:as) -> put (a + b : as); _        -> return ();
eval = do ins <- ask; case ins of [] -> return (); (i:is) -> do evalCode i; local tail eval; return ()
