{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
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
              ) where

import Control.Applicative  (Alternative((<|>)))
import Control.Lens         ((<>~))
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson           (FromJSON, ToJSON, decode, encode)
import Data.List            (lookup)
import GHC.TypeLits         (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import Hasql.Pool           (Pool)
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (natVal, state)
import Web.HttpApiData      (FromHttpApiData(parseHeader, parseUrlPiece))

import Errors
    ( AuthError
    , ServerError(ServerError)
    , err404
    , mkError
    , toResponse
    )
import Types.Router

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

class (MonadIO m, MonadError ServerError m) => RoutingConstraint m
instance (MonadIO m, MonadError ServerError m) => RoutingConstraint m

type family Server (layout :: Type) where
  Server layout = ServerT layout Handler

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b = ()
  Or a () = ()

type family IsThere (endpoint :: Type) (api :: Type) :: Constraint where
  IsThere e (r1 :<|> r2) = Or (IsThere e r1) (IsThere e r2)
  IsThere (e :> ep) (e :> r) = IsThere ep r
  IsThere e (ReqBody f a :> r) = IsThere e r
  IsThere e (QueryParam f a :> r) = IsThere e r
  IsThere (Capture d a :> e) (Capture f a :> r) = IsThere e r
  IsThere e e = ()

handleAuth :: RoutingConstraint m => Auth
                                  -> Maybe ( ByteString
                                          -> IO (Either AuthError Auth)
                                           ) -> m Auth
handleAuth NoAuth _ = return NoAuth
handleAuth _ Nothing = return NoAuth
handleAuth _ _ = undefined

fac n = fac' n id
  where
    fac' 0 c = c 1
    fac' n c = fac' (n - 1) \x -> c (n * x)

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r
class HasServer layout where
  type ServerT (layout :: Type) (m :: Type -> Type) :: Type
  route  :: RoutingConstraint m => ServerT layout m -> RoutingEnv -> Maybe (m Response)
  unlift :: (forall x. m x -> n x) -> ServerT layout m -> ServerT layout n

instance ( KnownMethod t, KnownNat code, ToJSON a
          ) => HasServer (Verb t code 'JSON a) where
  type ServerT (Verb t code 'JSON a) m = m a
  route :: RoutingConstraint m => m a -> RoutingEnv -> Maybe (m Response)
  route _ (view method -> m) | parseMethod m /= Right (methodVal @t) = Nothing
  route handler req@(view authF -> howToAuth) = Just do
    handleAuth (req^.auth) howToAuth
    handler <&> responseLBS (createStatus @code) defaultHeaders . encode
  unlift :: (forall x. m x -> n x) -> m a -> n a
  unlift f = f

instance (HasServer r, FromJSON a) => HasServer (ReqBody 'JSON a :> r) where
  type ServerT (ReqBody 'JSON a :> r) m = a -> ServerT r m
  route :: RoutingConstraint m => (a -> ServerT r m) -> RoutingEnv -> Maybe (m Response)
  route f req@(view body -> bodyStr) = case decode @a bodyStr of
    Nothing -> Nothing
    Just a  -> route @r (f a) req
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type ServerT (ReqBody 'Raw BL.ByteString :> r) m = BL.ByteString -> ServerT r m
  route :: RoutingConstraint m => (BL.ByteString -> ServerT r m) -> RoutingEnv -> Maybe (m Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req
  unlift :: (forall x. m x -> n x) -> (BL.ByteString -> ServerT r m)
         -> (BL.ByteString -> ServerT r n)
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
  route :: RoutingConstraint m => m BL.ByteString -> RoutingEnv -> Maybe (m Response)
  route _ (view method -> m) | parseMethod m /= Right (methodVal @t) = Nothing
  route handler _ = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders
  unlift :: (forall x. m x -> n x) -> m BL.ByteString
         -> n BL.ByteString
  unlift f = f

instance (KnownSymbol s, FromHttpApiData a, HasServer r
          ) => HasServer (QueryParam s a :> r) where
  type ServerT (QueryParam s a :> r) m = Maybe a -> ServerT r m
  route :: RoutingConstraint m => (Maybe a -> ServerT r m) -> RoutingEnv -> Maybe (m Response)
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of
        Left err -> Nothing
        Right a  -> route @r (f $ Just a) req
      _ -> route @r (f Nothing) req
  unlift :: (forall x. m x -> n x) -> (Maybe a -> ServerT r m)
         -> (Maybe a -> ServerT r n)
  unlift f g = unlift @r f . g


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  route :: RoutingConstraint m => ServerT a m :<|> ServerT b m -> RoutingEnv -> Maybe (m Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req
  unlift :: (forall x. m x -> n x) -> (ServerT a m :<|> ServerT b m)
         -> (ServerT a n :<|> ServerT b n)
  unlift f (g1 :<|> g2) = unlift @a f g1 :<|> unlift @b f g2

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type ServerT ((s :: Symbol) :> r) m = ServerT r m
  route :: RoutingConstraint m => ServerT r m -> RoutingEnv -> Maybe (m Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> ServerT r m -> ServerT r n
  unlift f = unlift @r f

instance (FromHttpApiData a, HasServer r, KnownSymbol id
          ) => HasServer (Capture id a :> r) where
  type ServerT (Capture id a :> r) m = a -> ServerT r m
  route :: RoutingConstraint m => (a -> ServerT r m) -> RoutingEnv -> Maybe (m Response)
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> Nothing
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g

serve :: forall layout. HasServer layout => Server layout -> Application
serve s req respond = do
  bodyStr <- strictRequestBody req
  let reqInfo = RoutingEnv { _path = pathInfo req
                           , _method = requestMethod req
                           , _queryStr = queryString req
                           , _headers = requestHeaders req
                           , _body = bodyStr
                           , _auth = NoAuth
                           , _authF = Nothing
                           , _state = Nothing
                           }
  case route @layout s reqInfo of
    Nothing -> respond $ toResponse err404
    Just handler -> do
      let safeHandler = handler `catch` \(e :: SomeException) ->
            return $ toResponse (mkError status500 (fromString (show e)))
      runExceptT (runHandler safeHandler)
        >>= either (respond . toResponse) respond

serveWithAuth :: forall layout. HasServer layout =>
  (ByteString -> IO (Either AuthError Auth)) -> Server layout -> Application
serveWithAuth authF s req respond = do
  bodyStr <- strictRequestBody req
  let reqInfo = RoutingEnv { _path = pathInfo req
                           , _method = requestMethod req
                           , _queryStr = queryString req
                           , _headers = requestHeaders req
                           , _body = bodyStr
                           , _auth = NoAuth
                           , _authF = Just authF
                           , _state = Nothing
                           }
  case route @layout s reqInfo of
    Nothing -> respond $ toResponse err404
    Just handler -> do
      let safeHandler = handler `catch` \(e :: SomeException) ->
            return $ toResponse $ mkError status500 $ fromString $ show e
      runExceptT (runHandler safeHandler)
        >>= either (respond . toResponse) respond
