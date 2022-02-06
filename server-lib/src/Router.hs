{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Universum            hiding (Handle, natVal, state)
import Web.HttpApiData      (FromHttpApiData(parseHeader, parseUrlPiece))

import Errors
import Types.Router

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

--class (MonadIO m, MonadError ServerError m, FromAuth a) => RoutingConstraint m a a
--instance (MonadIO m, MonadError ServerError m, FromAuth a) => RoutingConstraint m a a
type RoutingConstraint m a = (MonadIO m, MonadError ServerError m, FromAuth a)

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

--fac :: (Eq p, Num p) => p -> p
--fac n = fac' n id
--  where
--    fac' 0 c = c 1
--    fac' n c = fac' (n - 1) \x -> c (n * x)

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r
class HasServer layout where
  type ServerT (layout :: Type) (m :: Type -> Type) :: Type
  route  :: RoutingConstraint m a => ServerT layout m -> RoutingEnv a -> Maybe (m Response)
  unlift :: (forall x. m x -> n x) -> ServerT layout m -> ServerT layout n
  walk :: (Path, StdMethod) -> Bool

class FromAuth a where
  from :: (ByteString, ByteString, ByteString) -> a

instance ( KnownMethod t, KnownNat code, ToJSON a
          ) => HasServer (Verb t code 'JSON a) where
  type ServerT (Verb t code 'JSON a) m = m a
  route :: RoutingConstraint m b => m a -> RoutingEnv b -> Maybe (m Response)
  route _ (view path -> p)   | not . null $ p                        = Nothing
  route handler req@(view authenticate -> auth) = Just do
--    handleAuth (req^.auth) howToAuth
    handler <&> responseLBS (createStatus @code) defaultHeaders . encode
  unlift :: (forall x. m x -> n x) -> m a -> n a
  unlift f = f
  walk :: (Path, StdMethod) -> Bool
  walk ([], m) | m == methodVal @t = True
  walk _ = False

instance ( KnownMethod t, KnownNat code
          ) => HasServer (Verb t code 'Raw BL.ByteString) where
  type ServerT (Verb t code 'Raw BL.ByteString) m = m BL.ByteString
  route :: RoutingConstraint m a => m BL.ByteString -> RoutingEnv a -> Maybe (m Response)
  route handler _ = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders
  unlift :: (forall x. m x -> n x) -> m BL.ByteString
         -> n BL.ByteString
  unlift f = f
  walk :: (Path, StdMethod) -> Bool
  walk ([], m) | m == methodVal @t = True
  walk _ = False

instance (HasServer r, FromJSON a) => HasServer (ReqBody 'JSON a :> r) where
  type ServerT (ReqBody 'JSON a :> r) m = a -> ServerT r m
  route :: RoutingConstraint m b => (a -> ServerT r m) -> RoutingEnv b -> Maybe (m Response)
  route f req@(view body -> bodyStr) = case decode @a bodyStr of
    Nothing -> Nothing
    Just a  -> route @r (f a) req
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: (Path, StdMethod) -> Bool
  walk = walk @r

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type ServerT (ReqBody 'Raw BL.ByteString :> r) m = BL.ByteString -> ServerT r m
  route :: RoutingConstraint m a => (BL.ByteString -> ServerT r m) -> RoutingEnv a -> Maybe (m Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req
  unlift :: (forall x. m x -> n x) -> (BL.ByteString -> ServerT r m)
         -> (BL.ByteString -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: (Path, StdMethod) -> Bool
  walk = walk @r

createStatus :: forall code. KnownNat code => Status
createStatus = case natVal (Proxy @code) of
  200 -> status200
  204 -> status204                                     --FIXME|
  n   -> mkStatus (fromInteger (natVal (Proxy @code))) "Success"

defaultHeaders :: [(HeaderName, ByteString)]
defaultHeaders = [("Content-type", "application/json")]

instance (KnownSymbol s, FromHttpApiData a, HasServer r
          ) => HasServer (QueryParam s a :> r) where
  type ServerT (QueryParam s a :> r) m = Maybe a -> ServerT r m
  route :: RoutingConstraint m b => (Maybe a -> ServerT r m) -> RoutingEnv b -> Maybe (m Response)
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of
        Left err -> Nothing
        Right a  -> route @r (f $ Just a) req
      _ -> route @r (f Nothing) req
  unlift :: (forall x. m x -> n x) -> (Maybe a -> ServerT r m)
         -> (Maybe a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: (Path, StdMethod) -> Bool
  walk = walk @r

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  route :: RoutingConstraint m c => ServerT a m :<|> ServerT b m -> RoutingEnv c -> Maybe (m Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req
  unlift :: (forall x. m x -> n x) -> (ServerT a m :<|> ServerT b m)
         -> (ServerT a n :<|> ServerT b n)
  unlift f (g1 :<|> g2) = unlift @a f g1 :<|> unlift @b f g2
  walk :: (Path, StdMethod) -> Bool
  walk p = not (walk @a p) || walk @b p

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type ServerT ((s :: Symbol) :> r) m = ServerT r m
  route :: RoutingConstraint m a => ServerT r m -> RoutingEnv a -> Maybe (m Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> ServerT r m -> ServerT r n
  unlift f = unlift @r f
  walk :: (Path, StdMethod) -> Bool
  walk (x:xs, m) | T.unpack x == symbolVal (Proxy @s) = walk @r (xs, m)
  walk _ = False

instance (FromHttpApiData a, HasServer r, KnownSymbol id
          ) => HasServer (Capture id a :> r) where
  type ServerT (Capture id a :> r) m = a -> ServerT r m
  route :: RoutingConstraint m c => (a -> ServerT r m) -> RoutingEnv c -> Maybe (m Response)
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> Nothing
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: (Path, StdMethod) -> Bool
  walk (x:xs, m) = case parseUrlPiece @a x of
    Left _  -> False
    Right _ -> walk @r (xs, m)
  walk _ = False

instance (HasServer r, FromAuth a) => HasServer (AuthUser a :> r) where
  type ServerT (AuthUser a :> r) m = a -> ServerT r m
  route :: (MonadIO m, MonadError ServerError m) => (a -> ServerT r m) -> RoutingEnv a -> Maybe (m Response)
  route handlerF req@(lookup "Authentication" . view headers -> Just rawToken) =
    case req^.authHandle.extractToken $ rawToken of
      Nothing    -> throwError err403TokenInvalid
      Just token -> route (liftIO (req^.authHandle.auth $ token) >>= handlerF) req
  route _ _ = throwError err401
  unlift f g = unlift @r f . g
  walk = undefined

serve :: forall layout a. (HasServer layout, FromAuth a) =>
  Handle a -> Server layout -> Application
serve auth s (requestMethod -> m) respond
  | Left err <- parseMethod m = respond $ toResponse err400 -- FIXME bad error, non specific
serve auth s req@(requestMethod -> m) respond
  | Right m <- parseMethod m
  , True <- walk @layout (pathInfo req, m) = do
  bodyStr <- strictRequestBody req
  let reqInfo = RoutingEnv { _path = pathInfo req
                           , _method = m
                           , _queryStr = queryString req
                           , _headers = requestHeaders req
                           , _body = bodyStr
                           , _authHandle = auth
                           }
  case route @layout s reqInfo of
    Nothing -> respond $ toResponse err404
    Just handler -> do
      let safeHandler = handler `catch` \(e :: SomeException) ->
            return $ toResponse (mkError status500 (fromString (show e)))
      runExceptT (runHandler safeHandler)
        >>= either (respond . toResponse) respond
serve _ _ _ respond = respond $ toResponse err404

-- теперь мы знаем есть ли эндпоинт
-- то есть если не парсится рекбади - то это бэдреквест
-- эту информацию (где ошибка) можно нести в том типе, который в ExceptT
-- и делать хорошие сообщения об ошибках
{-
 serve :: forall layout. HasServer layout => Server layout -> Application
 serve s (requestMethod -> m) respond     | Left err <- parseMethod m =
   respond $ toResponse err400 -- FIXME bad error, non specific
 serve s req@(requestMethod -> m) respond | Right m <- parseMethod m
                                          , True <- walk @layout (pathInfo req, m) = do
   bodyStr <- strictRequestBody req
   let reqInfo = RoutingEnv a { _path = pathInfo req
                            , _method = m
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
 serve _ _ respond = respond $ toResponse err404
-}
