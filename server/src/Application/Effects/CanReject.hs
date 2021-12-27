{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.CanReject where

import Universum

import Control.Monad.Except (MonadError(throwError))
import Infrastructure

class MonadError ServerError m => CanReject m where
  reject :: ServerError -> m a
  reject = throwError

instance MonadError ServerError m => CanReject m
