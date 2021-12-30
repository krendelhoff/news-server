{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Authors where

import Universum

import Application.Effects.Auth
import Infrastructure

import Application
import Application.Authors
import Types.Authors
import Types.Environment

import qualified Application.Authors as Authors
import qualified Types.Users         as Users

class AuthenticateAdmin m => PersistAuthor m where
  downgrade :: ID -> m NoContent
  update    :: ID -> Description -> m Payload
  get       :: ID -> m (Maybe Payload)

instance (HasPersistAuthor env (Authors.Handle m), AuthenticateAdmin (AppM env m)
          , Monad m ) => PersistAuthor (AppM env m) where
  downgrade aid = do
    !admin <- whoami
    authorsHandle <- view persistAuthor
    lift $ view ldowngrade authorsHandle aid
  get aid = do
    !admin <- whoami
    authorsHandle <- view persistAuthor
    lift $ view lget authorsHandle aid
  update aid desc = do
    !admin <- whoami
    authorsHandle <- view persistAuthor
    lift $ view lupdate authorsHandle aid desc
