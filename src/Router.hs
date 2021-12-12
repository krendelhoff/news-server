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
import Control.Monad.Except
import Data.Aeson
import Data.List            (lookup)
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (error, natVal)
import Web.HttpApiData
import DB (run)

import Types.Environment
import Types.Router

import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Text            as T

error :: Response
error = responseLBS status404 [] "404!"

class HasServer layout where
  route :: Server layout -> RequestInfo -> Handler Response

instance ( KnownMethod m, KnownNat code, ToJSON a
         ) => HasServer (Verb m code a) where
  route :: Handler a -> RequestInfo -> Handler Response
  route handler _ = do
    -- AUTH Here
    run $ undefined
    -- Method routhing here
    handler <&>
      responseLBS (mkStatus (fromInteger (natVal (Proxy @code))) "") [] . encode

instance ( FromJSON a, HasServer r ) => HasServer (ReqBody a :> r) where
  route :: (a -> Server r) -> RequestInfo -> Handler Response
  route f req@(view body -> bodyStr) = case decodeStrict bodyStr of
    Nothing -> throwError CriticalError
    Just a  -> route @r (f a) req

instance (KnownSymbol s, FromHttpApiData a, HasServer r) =>
    HasServer (QueryParam s a :> r) where
    route :: (Maybe a -> Server r) -> RequestInfo -> Handler Response
    route f req@(view queryStr -> params) =
      case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
        Just (Just x) -> case parseHeader x of -- because of we need byteString instead of text!
          Left err -> throwError CriticalError
          Right a  -> route @r (f $ Just a) req
        _ -> route @r (f Nothing) req


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Server a :<|> Server b -> RequestInfo -> Handler Response
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Server r -> RequestInfo -> Handler Response
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = throwError CriticalError

instance (FromHttpApiData a, HasServer r) => HasServer (Capture a :> r) where
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
    Left err -> throwM CriticalError
    Right m  -> return m
  let reqInfo = RequestInfo { _path = pathInfo req
                            , _method = m
                            , _queryStr = queryString req
                            , _headers = requestHeaders req
                            , _body = bodyStr
                            }
  runExceptT (runReaderT (runHandler (route @layout s reqInfo)) ?env)
    >>= either (const $ respond error) respond
