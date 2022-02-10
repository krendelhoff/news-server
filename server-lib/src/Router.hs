{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Router ( HasServer(..)
              , serve
              , FromAuth(fromRawAuth)
              ) where

import Control.Applicative  (Alternative((<|>)))
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson           (FromJSON, ToJSON, decode, encode)
import Data.List            (lookup)
import Data.Validation
import GHC.IO               (unsafePerformIO)
import GHC.TypeLits         (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import Hasql.Pool           (Pool)
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (Handle, natVal, state)
import Web.HttpApiData      (FromHttpApiData(parseHeader, parseUrlPiece))

import Errors
import Types.Router
import Types.TH     (fromText)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

--class (MonadIO m, MonadError ServerError m, FromAuth a) => RoutingConstraint m a a
--instance (MonadIO m, MonadError ServerError m, FromAuth a) => RoutingConstraint m a a
--type RoutingConstraint m a = (MonadIO m, MonadError ServerError m, FromAuth a)

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
  -- don't know how to use it without constructing singleton paths

--fac :: (Eq p, Num p) => p -> p
--fac n = fac' n id
--  where
--    fac' 0 c = c 1
--    fac' n c = fac' (n - 1) \x -> c (n * x)

class FromAuth a where
  fromRawAuth :: RawAuthData -> a

-- TODO
-- routing
-- integration tests
-- more beautiful unit tests
-- readme (test data + request examples (only examples) and codes)
-- one more endpoint delete user

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r
class HasServer layout where
  type ServerT (layout :: Type) (m :: Type -> Type) :: Type
  route  :: Server layout -> RoutingEnv -> Maybe (Handler Response)
  unlift :: (forall x. m x -> n x) -> ServerT layout m -> ServerT layout n
  walk   :: RoutingEnv -> Maybe [RawRoute]

createStatus :: forall code. KnownNat code => Status
createStatus = case natVal (Proxy @code) of
  200 -> status200
  204 -> status204                                     --FIXME|
  n   -> mkStatus (fromInteger (natVal (Proxy @code))) "Success"

defaultHeaders :: [(HeaderName, ByteString)]
defaultHeaders = [("Content-type", "application/json; charset=utf-8")]

instance (KnownMethod t, KnownNat code, ToJSON a
          ) => HasServer (Verb t code 'JSON a) where
  type ServerT (Verb t code 'JSON a) m = m a
  route :: Handler a -> RoutingEnv -> Maybe (Handler Response)
  route _ (view path -> p)   | not . null $ p = Nothing
  route handler (view method -> m) | m == methodVal @t = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders . encode
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> m a -> n a
  unlift f = f
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk req@(view method -> m) | m == methodVal @t = Just []
  walk _ = Nothing

instance (KnownMethod t, KnownNat code
          ) => HasServer (Verb t code 'Raw BL.ByteString) where
  type ServerT (Verb t code 'Raw BL.ByteString) m = m BL.ByteString
  route :: Handler BL.ByteString -> RoutingEnv -> Maybe (Handler Response)
  route handler (view method -> m) | m == methodVal @t = Just do
    handler <&> responseLBS (createStatus @code) defaultHeaders
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> m BL.ByteString
         -> n BL.ByteString
  unlift f = f
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk (view method -> m) | m == methodVal @t = Just []
  walk _ = Nothing

parseReqB :: forall a. FromJSON a => BL.ByteString -> Either [ErrorMessage] a
parseReqB = maybe (Left ["Can't parse request body"]) Right . decode @a

instance (HasServer r, FromJSON a) => HasServer (ReqBody 'JSON a :> r) where
  type ServerT (ReqBody 'JSON a :> r) m = a -> ServerT r m
  route :: (a -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = case parseReqB @a bodyStr of
    Right a  -> route @r (f a) req
    _        -> Nothing
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk req@(view body -> bodyStr) = walk @r req <&> (RawReqB (Proxy @a) bodyStr :)

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type ServerT (ReqBody 'Raw BL.ByteString :> r) m = BL.ByteString -> ServerT r m
  route :: (BL.ByteString -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req
  unlift :: (forall x. m x -> n x) -> (BL.ByteString -> ServerT r m)
         -> (BL.ByteString -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk = walk @r

parseQueryParam :: forall a. FromHttpApiData a => String -> ByteString
                                               -> Either [ErrorMessage] a
parseQueryParam s =
  either (Left . return . (("Can't parse query parameter " <> fromString s <> ": ") <>) . fromText)
         Right  . parseHeader @a

instance (KnownSymbol s, FromHttpApiData a, HasServer r
          ) => HasServer (QueryParam s a :> r) where
  type ServerT (QueryParam s a :> r) m = Maybe a -> ServerT r m
  route :: (Maybe a -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route f req@(view queryStr -> params) = do
   let param = symbolVal (Proxy @s)
   case lookup (encodeUtf8 param) params of
     Just (Just x) -> case parseQueryParam @a param x of
       (Right a) -> route @r (f $ Just a) req
       _         -> Nothing
     _ -> route @r (f Nothing) req
  unlift :: (forall x. m x -> n x) -> (Maybe a -> ServerT r m)
         -> (Maybe a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) ->
        walk @r req <&> (RawQueryP (symbolVal (Proxy @s)) (Proxy @a) x :)
      _ -> walk @r req

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  route :: Server a :<|> Server b -> RoutingEnv -> Maybe (Handler Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req
  unlift :: (forall x. m x -> n x) -> (ServerT a m :<|> ServerT b m)
         -> (ServerT a n :<|> ServerT b n)
  unlift f (g1 :<|> g2) = unlift @a f g1 :<|> unlift @b f g2
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk p = walk @a p <|> walk @b p

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type ServerT ((s :: Symbol) :> r) m = ServerT r m
  route :: Server r -> RoutingEnv -> Maybe (Handler Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> ServerT r m -> ServerT r n
  unlift f = unlift @r f
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk req@(view path -> (x:xs))
    | T.unpack x == symbolVal (Proxy @s) = walk @r (req & path .~ xs)
  walk _ = Nothing

parseCapture :: forall a. FromHttpApiData a => String -> Text
                                            -> Either [ErrorMessage] a
parseCapture s =
  either (Left . return . (("Can't parse capture " <> fromString s <> ": ") <>) . fromText)
         Right . parseUrlPiece @a

instance (FromHttpApiData a, HasServer r, KnownSymbol id
          ) => HasServer (Capture id a :> r) where
  type ServerT (Capture id a :> r) m = a -> ServerT r m
  route :: (a -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route handler req@(view path -> (x:xs)) =
   case parseCapture @a (symbolVal (Proxy @id)) x of
     Left _ -> Nothing
     Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> (a -> ServerT r m)
         -> (a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk req@(view path -> (x:xs)) =
    walk @r (req & path .~ xs)
    <&> (RawCapt (symbolVal (Proxy @id)) (Proxy @a) x :)
  walk _ = Nothing

instance (FromAuth a, HasServer r) => HasServer (Auth a :> r) where
  type ServerT (Auth a :> r) m = AuthResult a -> ServerT r m
  route :: (AuthResult a -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route handlerF req@(lookup "Authorization" . view headers -> Just rawToken) =
    case _extractToken (req^.handle) rawToken of
      Nothing    -> route @r (handlerF $ fromRawAuth <$> BadToken) req
      Just token -> do
        authResult <- Just $ unsafePerformIO $ _authenticate (req^.handle) token
        route @r (handlerF $ fromRawAuth <$> authResult) req
  route handlerF req = route @r (handlerF $ fromRawAuth <$> NoToken) req
  unlift :: (forall x. m x -> n x) -> (AuthResult a -> ServerT r m)
                                   -> (AuthResult a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe [RawRoute]
  walk = walk @r

parseRoute :: [RawRoute] -> Validation [ErrorMessage] [Route]
parseRoute = foldr (\x acc ->
                       case x of
                         RawQueryP s p b -> liftA2 (:) (fromEither (parseQueryParamRoute s p b)) acc
                         RawCapt   s p t -> liftA2 (:) (fromEither (parseCaptureRoute s p t)) acc
                         RawReqB   p b   -> liftA2 (:) (fromEither (parseReqBRoute p b)) acc
                    ) (pure [])


parseReqBRoute :: forall a. FromJSON a => Proxy a -> BL.ByteString -> Either [ErrorMessage] Route
parseReqBRoute _ = (ReqB <$>) . parseReqB @a

parseQueryParamRoute :: forall a. FromHttpApiData a => String -> Proxy a
                                                    -> ByteString -> Either [ErrorMessage] Route
parseQueryParamRoute s _ = (QueryP @a s <$>) . parseQueryParam s
parseCaptureRoute :: forall a. FromHttpApiData a => String -> Proxy a
                                                 -> Text   -> Either [ErrorMessage] Route
parseCaptureRoute s _ = (Capt @a s <$>) . parseCapture s

-- TODO beautiful CODE MAN
-- TODO fix routing - recursive query param incorrect leads to not found
serve :: forall layout. HasServer layout =>
  ServingHandle -> Server layout -> Application
serve h s req@(requestMethod -> rawMethod) respond =
  case parseMethod rawMethod of
    Left err -> respond $ toResponse err400BadMethod
    Right m  -> do
      bodyStr <- strictRequestBody req
      let reqInfo = RoutingEnv { _path     = pathInfo req
                               , _method   = m
                               , _queryStr = queryString req
                               , _headers  = requestHeaders req
                               , _body     = bodyStr
                               , _handle   = h
                               , _racc     = []
                               }
      case walk @layout reqInfo of
        Nothing       -> respond $ toResponse err404
        Just rawRacc  -> case parseRoute rawRacc of
          Failure errs -> respond $ toResponse (mkError status400 errs)
          Success r    -> do
            case route @layout s (reqInfo & racc .~ r) of
              Nothing -> respond $ toResponse err404
              Just handler -> do
                let safeHandler = handler `catch` \(e :: SomeException) -> do
                      liftIO $ _log h $ fromString $ displayException e
                      return $ toResponse err500
                runExceptT (runHandler safeHandler)
                  >>= either (respond . toResponse) respond
