{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Control.Monad.Loop
  ( for
  , while
  , With(..)
  , with_
  , withi_
  , withWhile_
  , withWhilei_
  , quit
  , cease
  )
  where

-- This library provides a simple dsl that mimics imperative loop.

-- import           Control.Monad.Loop.Class    (With (..))
import           Control.Monad.Loop.Internal (cease, for, quit, while,
                                              withWhile_, withWhilei_, with_,
                                              withi_)

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Loop.Internal (Loop)


class With m ret param where
  with :: Loop m ret -> (param -> ExceptT () m ()) -> m ()

instance (Monad m, Traversable t, Integral n) => With m (t a, a -> Bool) (n, a) where
    with = withWhilei_

instance (Monad m, Traversable t) => With m (t a, a -> Bool) a where
    with = withWhile_

instance (Monad m, Traversable t, Integral n) => With m (t a) (n, a) where
    with = withi_

instance (Monad m, Traversable t) => With m (t a) a where
    with = with_
