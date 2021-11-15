{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Control.Monad.Loop.Internal where

import           Control.Monad.Except  (ExceptT, MonadError (throwError),
                                        runExceptT)
import           Control.Monad.ST.Lazy (runST)
import           Data.Foldable         (traverse_)
import           Data.Kind             (Type)
import           Data.STRef.Lazy       (modifySTRef, newSTRef, readSTRef)


type Loop :: (Type -> Type) -> Type -> Type
data Loop m a where
  For    :: t a -> Loop m (t a)
  While  :: Loop m (t a) -> (a -> Bool) -> Loop m (t a, a -> Bool)

-- | `for` clause to start a loop
for :: Traversable t => t a -> Loop m (t a)
for = For

{- | `while` clause to determine the terminal condition of a loop.

@
  for [(0::Int)..] \`while\` (\<10) \`with`\ \\(i::Int) -> lift do
    putStrLn "hi"
@

-}
while  :: Traversable t
       => Loop m (t a) -> (a -> Bool) -> Loop m (t a, a -> Bool)
while = While

evalLoop :: Monad m => Loop m a -> m a
evalLoop (For xs) = do
  return xs
evalLoop (While loop pred) = do
  xs <- evalLoop loop
  return (xs, pred)

-- | start a for-each style loop.
with_ :: (Traversable t, Monad m)
      => Loop m (t a) -> (a -> ExceptT () m ()) -> m ()
with_ loop k = do
  xs <- evalLoop loop
  runExceptT $ traverse_ k xs
  return ()

enumerateTrav :: (Traversable t, Integral n) => t a -> t (n, a)
enumerateTrav ts = runST $ do
  idxref <- newSTRef 0
  flip traverse ts $ \value -> do
    idx <- readSTRef idxref
    idxref `modifySTRef` (+ 1)
    return (idx, value)

-- | start a for-each style loop with access to indices.
withi_ :: (Traversable t, Monad m, Integral n)
       => Loop m (t a) -> ((n, a) -> ExceptT () m ()) -> m ()
withi_ loop k = do
  xs <- evalLoop loop
  runExceptT
    . traverse_ k . enumerateTrav
    $ xs
  return ()

-- | start a for-each style loop with while clause.
withWhile_ :: (Traversable t, Monad m)
           => Loop m (t a, a -> Bool) -> (a -> ExceptT () m ()) -> m ()
withWhile_ loop k = do
  (ts, pred) <- evalLoop loop
  runExceptT
    . traverse_ (\a -> if pred a then k a else throwError ())
    $ ts
  return ()

-- | start a for-each style loop with while clause and access to indices.
withWhilei_ :: (Traversable t, Monad m, Integral n)
           => Loop m (t a, a -> Bool) -> ((n, a) -> ExceptT () m ()) -> m ()
withWhilei_ loop k = do
  (ts, pred) <- evalLoop loop
  runExceptT
    . traverse_ (\(n, a) -> if pred a then k (n, a) else throwError ())
    . enumerateTrav
    $ ts
  return ()

-- | break to the outer loop.
quit :: Monad m => ExceptT () m a
quit = throwError ()

-- | break to the outer most loop.
cease :: Monad m => ExceptT () m a
cease = quit >> cease
