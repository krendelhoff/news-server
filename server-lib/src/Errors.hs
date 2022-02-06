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
import Network.HTTP.Types (Status, status401, status404, status500, status403, status400)
import Network.Wai        (Response, responseLBS)
import Universum

import qualified Hasql.Pool as Pool

-- HTTP

data ServerError = ServerError Status Message
  deriving (Eq, Show, Exception)

toResponse :: ServerError -> Response
toResponse (ServerError st m) =
  responseLBS st [("Content-type", "application/json; charset=utf-8")] (encode m)

mkError :: Status -> Message -> ServerError
mkError = ServerError

err404 :: ServerError
err404 = mkError status404 "Not found"

err400 :: ServerError
err400 = mkError status400 "Bad request"

err500 :: ServerError
err500 = mkError status500 "Internal Error"

err401 :: ServerError
err401 = mkError status401 "Unauthorized"

err403 :: ServerError
err403 = mkError status403 "Access denied"

err400BadMethod :: ServerError
err400BadMethod = mkError status400 "Bad method"

err400BadReqBody :: ServerError
err400BadReqBody = mkError status400 "Bad request body"

err400BadCapture :: String -> ServerError
err400BadCapture cid = mkError status400 $ "Bad " <> fromString cid

err400BadQueryParam :: String -> ServerError
err400BadQueryParam s = mkError status400 $ "Bad " <> fromString s

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
