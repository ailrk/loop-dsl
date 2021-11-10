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
{-# LANGUAGE DataKinds #-}
module Control.Monad.Loop.Class
  ( With(..) )
  where

-- This library provides a simple dsl that mimics imperative loop.

import Control.Monad.Loop.Internal
import Data.Kind
import Control.Monad.Except


class With m ret param where
  with :: Loop m ret -> (param -> ExceptT () m ()) -> m ()
