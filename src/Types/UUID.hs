{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Types.UUID where

import Data.Aeson.Types
import Data.UUID                       (UUID)
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Universum
import Web.PathPieces                  (PathPiece)

import qualified Data.ByteString.Char8 as B8
import qualified Data.UUID             as UUID

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . B8.pack . UUID.toString
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

deriving instance PathPiece UUID
