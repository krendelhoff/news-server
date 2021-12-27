{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Errors where

import Data.Aeson         (ToJSON, encode)
import Hasql.Connection   (ConnectionError)
import Hasql.Migration    (MigrationError)
import Network.HTTP.Types (Status, status401, status404, status500, status403)
import Network.Wai        (Response, responseLBS)
import Universum

import qualified Hasql.Pool as Pool


-- HTTP

data ServerError = ServerError Status Message
  deriving (Eq, Show, Exception)

toResponse :: ServerError -> Response
toResponse (ServerError st m) = responseLBS st [] (encode m)

mkError :: Status -> Message -> ServerError
mkError = ServerError

err404 :: ServerError
err404 = mkError status404 "Not found"

err500 :: ServerError
err500 = mkError status500 "Internal Error"

err401 :: ServerError
err401 = mkError status401 "Unauthorized"

err403 :: ServerError
err403 = mkError status403 "Access denied"

err403TokenExpired :: ServerError
err403TokenExpired = mkError status403 "Token expired"

err403TokenInvalid :: ServerError
err403TokenInvalid = mkError status403 "Token invalid"

data TokenError = NoToken | BadToken deriving (Eq, Show)

newtype Message = Message { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (IsString, Semigroup, Monoid)

-- Database

instance Exception ConnectionError
instance Exception MigrationError
instance Exception Pool.UsageError
