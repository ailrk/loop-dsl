{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
module Control.Monad.Loop
  ( across
  , loop
  , while
  , with_
  , withi_
  , withWhile_
  , withWhilei_
  , quit
  , module Control.Monad.Loop.Class
  )
  where

-- This library provides a simple dsl that mimics imperative loop.

import           Control.Monad.Except
import           Control.Monad.Loop.Class
import           Control.Monad.Loop.Internal
import           Data.Kind


instance (Monad m, Traversable t, Integral n) => With m (t a, a -> Bool) (n, a) where
    with = withWhilei_

instance (Monad m, Traversable t) => With m (t a, a -> Bool) a where
    with = withWhile_

instance (Monad m, Traversable t, Integral n) => With m (t a) (n, a) where
    with = withi_

instance (Monad m, Traversable t) => With m (t a) a where
    with = with_
