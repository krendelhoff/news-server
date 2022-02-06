{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Authors where

import Universum

import Infrastructure

import Application
import Types.Authors
import Types.Environment

import qualified Application.Authors as Authors
import qualified Types.Users         as Users

class PersistAuthor m where
  downgrade :: ID -> m NoContent
  update    :: ID -> Description -> m Payload
  get       :: ID -> m (Maybe Payload)

instance (HasPersistAuthor env (Authors.Handle m), Monad m
          ) => PersistAuthor (AppM env m) where
  downgrade aid = do
    authorsHandle <- view persistAuthor
    lift $ view Authors.downgrade authorsHandle aid
  get aid = do
    authorsHandle <- view persistAuthor
    lift $ view Authors.get authorsHandle aid
  update aid desc = do
    authorsHandle <- view persistAuthor
    lift $ view Authors.update authorsHandle aid desc
