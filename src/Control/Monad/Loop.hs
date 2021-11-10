{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE InstanceSigs #-}
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
    -- with :: Loop m r (t a, a -> Bool) -> ((n, a) -> ExceptT () m ()) -> m ()
    with = withWhilei_

instance (Monad m, Traversable t) => With m (t a, a -> Bool) a where
    -- with :: Loop m r (t a, a -> Bool) -> (a -> ExceptT () m ()) -> m ()
    with = withWhile_

instance (Monad m, Traversable t, Integral n) => With m (t a) (n, a) where
    -- with :: Loop m r (t a) -> ((n, a) -> ExceptT () m ()) -> m ()
    with = withi_

instance (Monad m, Traversable t) => With m (t a) a where
    -- with :: Loop m r (t a) -> (a -> ExceptT () m ()) -> m ()
    with = with_
