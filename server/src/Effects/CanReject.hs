{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Effects.CanReject where

import Universum

import Control.Monad.Except (MonadError(throwError))
import Infrastructure       (ServerError)

class MonadError ServerError m => CanReject m where
  reject :: ServerError -> m a
  reject = throwError

instance MonadError ServerError m => CanReject m
