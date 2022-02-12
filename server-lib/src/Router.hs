{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
module Router ( HasServer(..)
              , serve
              , FromAuth(fromRawAuth)
              , Elem
              ) where

import Data.Aeson           (FromJSON, ToJSON, decode, encode)
import Data.List            (lookup)
import Data.Validation
import GHC.TypeLits         (KnownSymbol, Symbol, natVal, symbolVal)
import Network.HTTP.Types
import Network.Wai
import Universum            hiding (Handle, natVal, state)
import Web.HttpApiData      (FromHttpApiData(parseHeader, parseUrlPiece))

import Errors
import Types.Router
import Types.TH     (fromText)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

type family Server (layout :: Type) where
  Server layout = ServerT layout Handler

type family Elem (lst :: [a]) (el :: a) where
  Elem '[] a       = 'False
  Elem (a ': xs) a = 'True
  Elem (x ': xs) a = Elem xs a

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

class FromAuth a where
  fromRawAuth :: RawAuthData -> a

class HasServer layout where
  type ServerT (layout :: Type) (m :: Type -> Type) :: Type
  route  :: Server layout -> RoutingEnv -> Maybe (Handler Response)
  unlift :: (forall x. m x -> n x) -> ServerT layout m -> ServerT layout n
  walk   :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)

createStatus :: forall code. KnownNat code => Status
createStatus = case natVal (Proxy @code) of
  200 -> status200
  204 -> status204                                     --FIXME|
  n   -> mkStatus (fromInteger n) "Success"

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
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk (view path -> p)       | not . null $ p    = Nothing
  walk (view method -> m) | m == methodVal @t = Just ([], Regular)
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
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk (view method -> m) | m == methodVal @t = Just ([], Regular)
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
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk req@(view body -> bodyStr) = walk @r req <&> first (RawReqB (Proxy @a) bodyStr :)

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type ServerT (ReqBody 'Raw BL.ByteString :> r) m = BL.ByteString -> ServerT r m
  route :: (BL.ByteString -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req
  unlift :: (forall x. m x -> n x) -> (BL.ByteString -> ServerT r m)
         -> (BL.ByteString -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk = walk @r

parseQueryParam :: forall a. FromHttpApiData a => String -> ByteString
                                               -> Either [ErrorMessage] a
parseQueryParam s =
  first (return . (("Can't parse query parameter " <> fromString s <> ": ") <>) . fromText)
        . parseHeader @a

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
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) ->
        walk @r req <&> first (RawQueryP (symbolVal (Proxy @s)) (Proxy @a) x :)
      _ -> walk @r req

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  route :: Server a :<|> Server b -> RoutingEnv -> Maybe (Handler Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req
  unlift :: (forall x. m x -> n x) -> (ServerT a m :<|> ServerT b m)
         -> (ServerT a n :<|> ServerT b n)
  unlift f (g1 :<|> g2) = unlift @a f g1 :<|> unlift @b f g2
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk p = walk @a p <|> walk @b p

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type ServerT ((s :: Symbol) :> r) m = ServerT r m
  route :: Server r -> RoutingEnv -> Maybe (Handler Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing
  unlift :: (forall x. m x -> n x) -> ServerT r m -> ServerT r n
  unlift f = unlift @r f
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk req@(view path -> (x:xs))
    | T.unpack x == symbolVal (Proxy @s) = walk @r (req & path .~ xs)
  walk _ = Nothing

parseCapture :: forall a. FromHttpApiData a => String -> Text
                                            -> Either [ErrorMessage] a
parseCapture s =
  first (return . (("Can't parse capture " <> fromString s <> ": ") <>) . fromText)
        . parseUrlPiece @a

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
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk req@(view path -> (x:xs)) =
    walk @r (req & path .~ xs) <&> first (RawCapt (symbolVal (Proxy @id)) (Proxy @a) x :)
  walk _ = Nothing

instance (FromAuth a, HasServer r, KnownProtection protection
          ) => HasServer (Auth protection a :> r) where
  type ServerT (Auth protection a :> r) m = AuthResult a -> ServerT r m
  route :: (AuthResult a -> Server r) -> RoutingEnv -> Maybe (Handler Response)
  route handlerF req@(view auth -> result) =
    route @r (handlerF $ fromRawAuth <$> result) req
  unlift :: (forall x. m x -> n x) -> (AuthResult a -> ServerT r m)
                                   -> (AuthResult a -> ServerT r n)
  unlift f g = unlift @r f . g
  walk :: RoutingEnv -> Maybe ([RawRoutePiece], Protection)
  walk = (second (max (protectionVal @protection)) <$>) . walk @r

authorize :: Request -> ServingHandle -> IO (AuthResult RawAuthData)
authorize (lookup "Authorization" . requestHeaders -> Just rawToken) h =
  case _extractToken h rawToken of
    Nothing    -> return BadToken
    Just token -> _authenticate h token
authorize _ _ = return NoToken

parseRoute :: [RawRoutePiece] -> Validation [ErrorMessage] [RoutePiece]
parseRoute =
  foldr (\x acc -> case x of
            RawQueryP s p b -> liftA2 (:) (fromEither (parseQueryParamRoutePiece s p b)) acc
            RawCapt   s p t -> liftA2 (:) (fromEither (parseCaptureRoutePiece s p t)) acc
            RawReqB   p b   -> liftA2 (:) (fromEither (parseReqBRoutePiece p b)) acc
          ) (pure [])
  where
    parseReqBRoutePiece :: forall a. FromJSON a => Proxy a -> BL.ByteString -> Either [ErrorMessage] RoutePiece
    parseReqBRoutePiece _ = (ReqB @a <$>) . parseReqB @a
    parseQueryParamRoutePiece :: forall a. FromHttpApiData a => String -> Proxy a
                                                             -> ByteString -> Either [ErrorMessage] RoutePiece
    parseQueryParamRoutePiece s _ = (QueryP @a s <$>) . parseQueryParam @a s
    parseCaptureRoutePiece :: forall a. FromHttpApiData a => String -> Proxy a
                                                          -> Text   -> Either [ErrorMessage] RoutePiece
    parseCaptureRoutePiece s _ = (Capt @a s <$>) . parseCapture @a s


serve :: forall layout. HasServer layout =>
  ServingHandle -> Server layout -> Application
serve _ _ (parseMethod . requestMethod -> Left _) respond =
  respond $ toResponse err400BadMethod
serve h s req@(parseMethod . requestMethod -> Right m) respond =
  serveWithMethod @layout h s m req respond
  where
    serveWithMethod :: forall layout. HasServer layout =>
      ServingHandle -> Server layout -> StdMethod -> Application
    serveWithMethod h s m req respond = do
      bodyStr    <- strictRequestBody req
      let reqInfo = RoutingEnv { _path     = pathInfo req
                               , _method   = m
                               , _queryStr = queryString req
                               , _headers  = requestHeaders req
                               , _body     = bodyStr
                               , _handle   = h
                               , _auth     = NoToken
                               }
      case walk @layout reqInfo of
        Nothing       -> respond $ toResponse err404
        Just rawRacc  -> serveAfterWalk @layout s reqInfo rawRacc req respond
    serveAfterWalk :: forall layout. HasServer layout =>
      Server layout -> RoutingEnv -> ([RawRoutePiece], Protection) -> Application
    serveAfterWalk _ env (parseRoute -> Failure errs, Protected) req respond = do
      authorize req (env^.handle) >>= respond . toResponse . flip handleErrs errs
    serveAfterWalk _ _ (parseRoute -> Failure errs, Regular) _ respond =
      respond $ toResponse (mkError status400 errs)
    serveAfterWalk s env _ req respond = do
      authResult <- authorize req (env^.handle)
      serveWithRoute @layout s (env & auth .~ authResult) req respond
    handleErrs :: AuthResult RawAuthData -> [ErrorMessage] -> ServerError
    handleErrs (AuthSuccess (RawAuthData _ _ _ True Access)) errs = mkError status400 errs
    handleErrs _ _                  = err404
    serveWithRoute :: forall layout. HasServer layout =>
      Server layout -> RoutingEnv -> Application
    serveWithRoute s env _ respond =
      case route @layout s env of
        Nothing -> respond $ toResponse err404
        Just handler -> do
          let safeHandler = handler `catch` \(e :: SomeException) -> do
                liftIO $ _log h $ fromString $ displayException e
                return $ toResponse err500
          runExceptT (runHandler safeHandler)
            >>= either (respond . toResponse) respond
serve _ _ _ respond = respond $ toResponse err404
