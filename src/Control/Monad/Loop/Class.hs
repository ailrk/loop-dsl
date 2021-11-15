{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Loop.Class
  ( With(..) )
  where

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Loop.Internal (Loop)


class With m ret param where
  with :: Loop m ret -> (param -> ExceptT () m ()) -> m ()
