
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad.Cont
import           Control.Monad.Loop
import           Control.Monad.Loop.Internal
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Writer
import           Data.Foldable
import           Data.IORef



import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed.Mutable as UV


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
  for [0..5] `with_` \i -> lift $ do eval

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

loopTestQuit = do
  for [(0::Int)..] `with` \(i::Int) -> do
    if i == 3 then quit else lift $ do
      putStr "loop 1"; putStrLn $ "=> " ++ show i
  for [(0 :: Int)..3] `with` \(i :: Int) -> lift $ do
    for [(0 :: Int)..3] `with` \(j :: Int) -> do
      if j == 2 then cease else
        lift $ putStrLn $ show i ++ " " ++ show j

loopTestMutation :: IO ()
loopTestMutation = do
  counter <- newIORef 0
  for [(0::Int)..] `while` (<3) `with` \(i::Int) -> lift $ do
    c <- readIORef counter
    putStrLn $ "old current counter is: " ++ show c
    counter `modifyIORef'` (+1)
    c <- readIORef counter
    putStrLn $ "new current counter is: " ++ show c

-- imperative nQueen
nQueen :: Int -> IO ()
nQueen n = do board <- newBoard; nSols <- newMVar 0; nqueen nSols board 0
  where
    newBoard = UV.replicate n ((-1) :: Int)
    nqueen nSols board i = flip runContT return . callCC $ \ret -> do
      let end g = do
            lift $ when g $ do
              nSols `modifyMVar_` (return . (+1))
              printBoard board
            ret ()
      let backtrack = ret ()
      let parallelSearch () = lift $ do -- IO [Async ()]
            threads <- sequence $
              [ do GM.unsafeWrite board i col
                   thread <- async $ do
                     nb <- G.thaw =<< G.freeze board
                     nqueen nSols nb (i + 1)
                   return thread
              | col <- [0..GM.length board] ]
            mapConcurrently_ wait threads
      let nestedSearch () = lift $ do
            for [(0::Int)..GM.length board] `with` \col -> lift $ do
              GM.unsafeWrite board i col
              nqueen nSols board (i + 1)
      g <- lift $ good board i
      when (i > GM.length board) $ do end g
      when (not g) $ backtrack
      if (i == 0) then parallelSearch () else nestedSearch ()
    threaten rA cA rB cB = rA == rB || cA == cB || abs (rA - rB) == abs (cA - cB)
    good board endIdx = do
      res <- newIORef True
      for [(0::Int)..endIdx-1] `with` \(rA :: Int) -> do
        for [rA+1..endIdx-1] `with` \(rB :: Int) -> do
          cA <- board `GM.unsafeRead` rA; cB <- board `GM.unsafeRead` rB
          when (threaten rA cA rB cB) $ lift . lift $ res `writeIORef` False
          cease
      readIORef res
    printBoard board = let loop = (for [(0::Int)..n-1] `with`) in do
      loop $ \(i::Int) -> lift $ do
        loop $ \(j::Int) -> lift $ do
          v <- board `GM.unsafeRead` i;
          putStr $ (if v == j then 'X' else '_') : " "
        putStrLn' ""

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
