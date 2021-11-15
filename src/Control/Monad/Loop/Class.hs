{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Loop.Class
  ( With(..) )
  where

-- This library provides a simple dsl that mimics imperative loop.

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Loop.Internal (Loop)


class With m ret param where
  with :: Loop m ret -> (param -> ExceptT () m ()) -> m ()
