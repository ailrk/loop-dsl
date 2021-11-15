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
  For :: Traversable t => t a -> Loop m (t a)
  While  :: Traversable t
         => Loop m (t a) -> (a -> Bool) -> Loop m (t a, a -> Bool)
  -- Run    :: Loop m ()

for :: Traversable t => t a -> Loop m (t a)
for = For

while  :: Traversable t
       => Loop m (t a) -> (a -> Bool) -> Loop m (t a, a -> Bool)
while = While

evalLoop :: Monad m => Loop m a -> m a
-- evalLoop Run = return ()
evalLoop (For xs) = do
  return xs
evalLoop (While loop pred) = do
  xs <- evalLoop loop
  return (xs, pred)

-- implementation for different `with`
type WithTy m r loopRet actParam =
  Loop m loopRet -> (actParam -> ExceptT () m ()) -> m ()


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

-- With clause

withi_ :: (Traversable t, Monad m, Integral n)
       => Loop m (t a) -> ((n, a) -> ExceptT () m ()) -> m ()
withi_ loop k = do
  xs <- evalLoop loop
  runExceptT
    . traverse_ k . enumerateTrav
    $ xs
  return ()

withWhile_ :: (Traversable t, Monad m)
           => Loop m (t a, a -> Bool) -> (a -> ExceptT () m ()) -> m ()
withWhile_ loop k = do
  (ts, pred) <- evalLoop loop
  runExceptT
    . traverse_ (\a -> if pred a then k a else throwError ())
    $ ts
  return ()

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
