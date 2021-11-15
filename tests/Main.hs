
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Debug.Trace
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
nQueen n = do
  board <- newBoard
  nSols <- newMVar 0
  nqueen nSols board 0
  putStrLn . show =<< takeMVar nSols
  where
    copyBoard board = G.thaw =<< G.freeze board
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

test1 :: IO ()
test1 = do
  for [(0::Int)..2] `with` \(rA :: Int) -> do
    for [rA+1..4] `with` \(rB :: Int) -> lift $ do
      for [rB+1..6] `with` \(rC :: Int) -> lift $ do
        liftIO $ putStrLn $ show (rA, rB)
        if (rA == 1 && rB == 3) then do
          liftIO $ putStrLn "cease!"
          cease
        else return ()

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
