{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Errors where

import Data.Aeson         (ToJSON, encode)
import Hasql.Connection   (ConnectionError)
import Hasql.Migration    (MigrationError(..))
import Network.HTTP.Types
    ( Status
    , status400
    , status401
    , status403
    , status404
    , status500
    )
import Network.Wai        (Response, responseLBS)
import Universum

import qualified Data.ByteString.Char8 as BC
import qualified Hasql.Pool            as Pool

import Types.TH (newTextType)

-- HTTP

newTextType "ErrorMessage"

data ServerError = ServerError Status Messages
  deriving (Eq, Show, Exception)

toResponse :: ServerError -> Response
toResponse (ServerError st m) =
  responseLBS st [("Content-type", "application/json; charset=utf-8")] (encode m)

mkError :: Status -> [ErrorMessage] -> ServerError
mkError st = ServerError st . Messages

err404 :: ServerError
err404 = mkError status404 ["Not found"]

err400 :: ServerError
err400 = mkError status400 ["Bad request"]

err500 :: ServerError
err500 = mkError status500 ["Internal Error"]

err401 :: ServerError
err401 = mkError status401 ["Unauthorized"]

err403 :: ServerError
err403 = mkError status403 ["Access denied"]

err400BadMethod :: ServerError
err400BadMethod = mkError status400 ["Bad method"]

err400BadReqBody :: ServerError
err400BadReqBody = mkError status400 ["Can't parse request body"]

err400BadCapture :: String -> ServerError
err400BadCapture cid = mkError status400 ["Bad " <> fromString cid]

err400BadQueryParam :: String -> ServerError
err400BadQueryParam s = mkError status400 ["Bad " <> fromString s]

err403TokenExpired :: ServerError
err403TokenExpired = mkError status401 ["Token expired"]

err401TokenInvalid :: ServerError
err401TokenInvalid = mkError status403 ["Token invalid"]

failGracefully :: SomeException -> IO a
failGracefully e = do
  putStrLn (displayException e)
  exitFailure

newtype Messages = Messages { messages :: [ErrorMessage] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (Semigroup, Monoid)

-- Database

instance Exception ConnectionError where
  displayException = maybe "" (("[Exception: ConnectionError] " <>) . BC.unpack)

instance Exception MigrationError where
  displayException (ScriptChanged s)    = "[Exception: ScriptChanged] "    <> s
  displayException (ScriptMissing s)    = "[Exception: ScriptMissing] "    <> s
  displayException (ChecksumMismatch s) = "[Exception: ChecksumMismatch] " <> s
  displayException NotInitialised       = "***[Exception: Database Migrations Not Initialized]"

instance Exception Pool.UsageError where
  displayException (Pool.ConnectionError connErr) = displayException connErr
  displayException (Pool.SessionError queryErr)   = displayException queryErr
