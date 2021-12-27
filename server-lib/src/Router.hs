{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Router ( HasServer(..)
              , serve
              , status400
              , status403
              , status401
              , status500
              ) where

import Control.Applicative  (Alternative((<|>)))
import Control.Lens         ((<>~))
import Control.Monad.Except (throwError)
import Data.Aeson           (FromJSON, ToJSON, decode, encode)
import Data.List            (lookup)
import GHC.TypeLits         (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import Hasql.Pool           (Pool)
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (natVal, state)
import Web.HttpApiData      (FromHttpApiData(parseHeader, parseUrlPiece))

import Errors       (err404, mkError, toResponse)
import Types.Router

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T


type family Server (layout :: Type) where
  Server layout = ServerT layout Handler

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r
class HasServer layout where
  type ServerT (layout :: Type) (m :: Type -> Type) :: Type
  route  :: Server layout -> RoutingInfo -> Maybe (Handler Response)
  unlift :: (forall x. m x -> Handler x) -> ServerT layout m -> Server layout

instance ( KnownMethod t, KnownNat code, ToJSON a
          ) => HasServer (Verb t code 'JSON a) where
  type ServerT (Verb t code 'JSON a) m = m a
  route :: Handler a -> RoutingInfo -> Maybe (Handler Response)
  route _ (view method -> m) | parseMethod m /= Right (methodVal @t) = Nothing
  route handler _ = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders . encode
  unlift :: (forall x. m x -> Handler x) -> m a -> Handler a
  unlift f = f

instance (HasServer r, FromJSON a) => HasServer (ReqBody 'JSON a :> r) where
  type ServerT (ReqBody 'JSON a :> r) m = a -> ServerT r m
  route :: (a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = case decode @a bodyStr of
    Nothing -> Nothing{-Just $ throwError $ -- FIXME problem here
      mkError status400 "Request body format violation"-}
    Just a  -> route @r (f a) req
  unlift :: (forall x. m x -> Handler x) -> (a -> ServerT r m)
         -> (a -> Server r)
  unlift f g = unlift @r f . g

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type ServerT (ReqBody 'Raw BL.ByteString :> r) m = BL.ByteString -> ServerT r m
  route :: (BL.ByteString -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req
  unlift :: (forall x. m x -> Handler x) -> (BL.ByteString -> ServerT r m)
         -> (BL.ByteString -> Server r)
  unlift f g = unlift @r f . g

createStatus :: forall code. KnownNat code => Status
createStatus = case natVal (Proxy @code) of
  200 -> status200
  204 -> status204                                     --FIXME|
  n   -> mkStatus (fromInteger (natVal (Proxy @code))) "Success"

defaultHeaders :: [(HeaderName, ByteString)]
defaultHeaders = [("Content-type", "application/json")]


instance ( KnownMethod t, KnownNat code
          ) => HasServer (Verb t code 'Raw BL.ByteString) where
  type ServerT (Verb t code 'Raw BL.ByteString) m = m BL.ByteString
  route :: Handler BL.ByteString -> RoutingInfo -> Maybe (Handler Response)
  route _ (view method -> m) | parseMethod m /= Right (methodVal @t) = Nothing
  route handler _ = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders
  unlift :: (forall x. m x -> Handler x) -> m BL.ByteString
         -> Handler BL.ByteString
  unlift f = f

instance (KnownSymbol s, FromHttpApiData a, HasServer r
          ) => HasServer (QueryParam s a :> r) where
  type ServerT (QueryParam s a :> r) m = Maybe a -> ServerT r m
  route :: (Maybe a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of
        Left err -> Nothing {-Just $ throwError $ mkError status400 $
            "Can't parse " <> fromString (symbolVal $ Proxy @s)
                           <> " query parameter"-}
        Right a  -> route @r (f $ Just a) req
      _ -> route @r (f Nothing) req
  unlift :: (forall x. m x -> Handler x) -> (Maybe a -> ServerT r m)
         -> (Maybe a -> Server r)
  unlift f g = unlift @r f . g


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  route :: Server a :<|> Server b -> RoutingInfo -> Maybe (Handler Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req
  unlift :: (forall x. m x -> Handler x) -> (ServerT a m :<|> ServerT b m)
         -> (Server a :<|> Server b)
  unlift f (g1 :<|> g2) = unlift @a f g1 :<|> unlift @b f g2

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type ServerT ((s :: Symbol) :> r) m = ServerT r m
  route :: Server r -> RoutingInfo -> Maybe (Handler Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> Handler x) -> ServerT r m -> Server r
  unlift f = unlift @r f

instance (FromHttpApiData a, HasServer r, KnownSymbol id
          , Show a
          ) => HasServer (Capture id a :> r) where
  type ServerT (Capture id a :> r) m = a -> ServerT r m
  route :: (a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> Nothing {-Just $ throwError $
      mkError status400 $ "Can't parse capture "
                       <> fromString (symbolVal $ Proxy @id) FIXME valid routes
                                                                   but not found-}
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> Handler x) -> (a -> ServerT r m)
         -> (a -> Server r)
  unlift f g = unlift @r f . g

serve :: forall layout. HasServer layout => Server layout -> Application
serve s req respond = do
  bodyStr <- strictRequestBody req
  let reqInfo = RoutingInfo { _path = pathInfo req
                            , _method = requestMethod req
                            , _queryStr = queryString req
                            , _headers = requestHeaders req
                            , _body = bodyStr
                            , _state = Nothing
                            }
  case route @layout s reqInfo of
    Nothing -> respond $ toResponse err404
    Just handler -> do
      let safeHandler = handler `catch` \(e :: SomeException) ->
            return $ toResponse (mkError status500 (fromString (show e)))
          authHeader = lookup "Authorization" (requestHeaders req)
      runExceptT (runReaderT (runHandler safeHandler) authHeader)
        >>= either (respond . toResponse) respond
