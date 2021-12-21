{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Errors where

import Data.Aeson
import Hasql.Connection
import Hasql.Migration
import Network.HTTP.Types
import Universum

import qualified Hasql.Pool as Pool

-- HTTP

data ServerError = ServerError Status Message
  deriving (Eq, Show, Exception)

mkError :: Status -> Message -> ServerError
mkError = ServerError

err404 :: ServerError
err404 = ServerError status404 "Not found"

err500 :: ServerError
err500 = ServerError status500 "Internal Error"

err401 :: ServerError
err401 = ServerError status401 "Unauthorized"

err403TokenExpired :: ServerError
err403TokenExpired = ServerError status401 "Token expired"

err403TokenInvalid :: ServerError
err403TokenInvalid = ServerError status401 "Token invalid"

data TokenError = NoToken | BadToken deriving (Eq, Show)

newtype Message = Message { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (IsString, Semigroup, Monoid)

-- Database

instance Exception ConnectionError
instance Exception MigrationError
instance Exception Pool.UsageError
