{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
module Router where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import DB                   (run)
import Data.Aeson
import Data.List            (lookup)
import Data.Word8           (isSpace)
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (natVal)
import Web.HttpApiData

import Database.Auth

import Types.Environment
import Types.Router
import Types.Auth
import Types.TH
import Common

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Text            as T

respond404 :: Message -> Handler Response
respond404 = return . responseLBS status404 [] . encode

-- FIXME hardcode
message404 :: Message
message404 = "Not found!"

message500 :: Message
message500 = "Critical Internal Error!"

-- NOTE: hardcoding cause library already has good messages in statuses
respond500 :: Message -> Handler Response
respond500 = return . responseLBS status500 [] . encode

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r

class HasServer layout where
  type Server (layout :: Type) :: Type
  route :: Server layout -> RequestInfo -> Handler Response

instance HasServer r => HasServer (RequireAdmin :> r) where
  type Server (RequireAdmin :> r) = Server r
  route :: Server r -> RequestInfo -> Handler Response
  route handler req@(view headers -> hMap) = case lookup "Authorization" hMap of
    Just (((fromText <$>) . decodeUtf8' <$>)
          . B.break isSpace -> ("Bearer", Right token)) -> do
      getCurrentTime >>= run . getTokenInfo token >>= \case
        Just (TokenInfo _ (toBool -> True) (toBool -> True)) ->
          error "respond401 Token expired"
        Just (TokenInfo _ (toBool -> True) (toBool -> False)) ->
          route @r handler (req & auth .~ True)
        _ -> respond404 message404
    _ -> respond404 message404 -- FIXME maybe Bad Request better here

instance ( KnownMethod m, KnownNat code, ToJSON a
         ) => HasServer (Verb m code a) where
  type Server (Verb m code a) = Handler a
  route :: Handler a -> RequestInfo -> Handler Response
  route handler _ = do
    -- AUTH Here
    run $ undefined
    -- Method routhing here
    handler <&>
      responseLBS (mkStatus (fromInteger (natVal (Proxy @code))) "") [] . encode

instance ( FromJSON a, HasServer r ) => HasServer (ReqBody a :> r) where
  type Server (ReqBody a :> r) = a -> Server r
  route :: (a -> Server r) -> RequestInfo -> Handler Response
  route f req@(view body -> bodyStr) = case decodeStrict bodyStr of
    Nothing -> throwError CriticalError
    Just a  -> route @r (f a) req

instance (KnownSymbol s, FromHttpApiData a, HasServer r) =>
    HasServer (QueryParam s a :> r) where
  type Server (QueryParam s a :> r) = Maybe a -> Server r
  route :: (Maybe a -> Server r) -> RequestInfo -> Handler Response
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of -- because of we need byteString instead of text!
        Left err -> throwError CriticalError
        Right a  -> route @r (f $ Just a) req
      _ -> route @r (f Nothing) req


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route :: Server a :<|> Server b -> RequestInfo -> Handler Response
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type Server ((s :: Symbol) :> r) = Server r
  route :: Server r -> RequestInfo -> Handler Response
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = throwError CriticalError

instance (FromHttpApiData a, HasServer r) => HasServer (Capture a :> r) where
  type Server (Capture a :> r) = a -> Server r
  route :: (a -> Server r) -> RequestInfo -> Handler Response
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> throwError WrongPath
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = throwError CriticalError

serve :: forall layout. (?env :: Environment, HasServer layout) => Server layout
                                                                -> Application
serve s req respond = do
  bodyStr <- BL.toStrict <$> strictRequestBody req -- FIXME bad desicion
  m <- case parseMethod (requestMethod req) of
    Left err -> throwM CriticalError -- FIXME NOT EXCEPTION BUT BAD REQUEST USE CALLCC
    Right m  -> return m
  let reqInfo = RequestInfo { _path = pathInfo req
                            , _method = m
                            , _queryStr = queryString req
                            , _headers = requestHeaders req
                            , _body = bodyStr
                            , _auth = False
                            }
  runExceptT
    (runReaderT
       (runHandler (do route @layout s reqInfo
                       `catchError` \case
                          WrongPath     -> respond404 message404
                          CriticalError -> respond500 message500
                    )) ?env)
    >>= either (const $ throwM CriticalError) respond
