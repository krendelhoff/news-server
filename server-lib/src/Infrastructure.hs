module Infrastructure ( module Router
                      , module Errors
                      , module DB
                      , module Types.Infrastructure
                      , (=>>)
                      ) where

import Universum

import DB
import Errors
import Router
import Types.Infrastructure

(=>>) :: Monad m => m a -> (a -> m b) -> m a
ma =>> f = do
  a <- ma
  f a
  return a
