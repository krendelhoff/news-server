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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
module Router where

import Control.Applicative
import Control.Lens        ((<>~))
import Control.Monad.Cont
import DB                  (run)
import Data.Aeson
import Data.List           (lookup)
import Data.Word8          (isSpace)
import GHC.TypeLits
import Hasql.Pool          (Pool)
import Network.HTTP.Types
import Network.Wai
import Universum           hiding (natVal)
import Web.HttpApiData

import Common
import Database.Auth
import Types.Auth
import Types.Common
import Types.Environment
import Types.Router
import Types.TH
import Types.Users (userId)

import qualified Types.Users as Users

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL (toStrict, ByteString)
import qualified Data.Text            as T


-- type instance Server (QueryParams s a :> r) = Vector a -> Server r

authenticate :: AuthLevel -> RequestHeaders
             -> ReaderT Pool Handler (Maybe Users.ID)
authenticate RequireAdmin headers =
  case extractToken headers of
    Right token -> do
      getCurrentTime >>= run . getTokenInfo token >>= \case
        Just (TokenInfo _ (toBool -> True) (toBool -> True) _) ->
          throwError err403TokenExpired
        Just (TokenInfo _ (toBool -> True) (toBool -> False) user) ->
          return $ Just user
        _ -> throwError err404
    _ -> throwError err404
authenticate RequireUser headers =
  case extractToken headers of
    Left NoToken -> throwError err401
    Left BadToken -> throwError err403TokenInvalid
    Right token -> do
      getCurrentTime >>= run . getTokenInfo token >>= \case
        Just (TokenInfo _ _ (toBool -> True) _) ->
          throwError err403TokenExpired
        Just (TokenInfo _ _ (toBool -> False) user) ->
          return $ Just user
        _ -> throwError err401
authenticate NoAuth _ = return Nothing


class HasServer layout where
  type Server (layout :: Type) :: Type
  route :: Server layout -> RoutingInfo -> Maybe (Handler Response)

instance HasServer r => HasServer (RequireAdmin :> r) where
  type Server (RequireAdmin :> r) = Server r
  route :: Server r -> RoutingInfo -> Maybe (Handler Response)
  route handler req = route @r handler (req & auth <>~ RequireAdmin)

instance HasServer r => HasServer (RequireUser :> r) where
  type Server (RequireUser :> r) = Server r
  route :: Server r -> RoutingInfo -> Maybe (Handler Response)
  route handler req = route @r handler (req & auth <>~ RequireUser)

instance (HasServer r, FromJSON a) => HasServer (ReqBody 'JSON a :> r) where
  type Server (ReqBody 'JSON a :> r) = a -> Server r
  route :: (a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = case decode @a bodyStr of
    Nothing -> Just $ throwError $
      mkError status400 "Request body format violation"
    Just a  -> route @r (f a) req

instance HasServer r => HasServer (ReqBody 'Raw BL.ByteString :> r) where
  type Server (ReqBody 'Raw BL.ByteString :> r) = BL.ByteString -> Server r
  route :: (BL.ByteString -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view body -> bodyStr) = route @r (f bodyStr) req

createStatus :: forall code. KnownNat code => Status
createStatus = case natVal (Proxy @code) of
  200 -> status200
  204 -> status204                                     --FIXME|
  n   -> mkStatus (fromInteger (natVal (Proxy @code))) "Success"


instance ( KnownMethod m, KnownNat code, ToJSON a
          ) => HasServer (Verb m code 'JSON a) where
  type Server (Verb m code 'JSON a) = AppM (Environment Handler) Handler a
  route :: AppM (Environment Handler) Handler a
        -> RoutingInfo -> Maybe (Handler Response)
  route _ (view method -> m) | m /= methodVal @m = Nothing
  route handler req@(view auth -> authLevel) = Just do
    runReaderT (authenticate authLevel (req^.headers)) (req^.env.pool)
      >>= maybe withNoAuthRespond withAuthRespond
    where
      withAuthRespond user =
        runReaderT (runAppM (handler <&>
          responseLBS (createStatus @code) defaultHeaders . encode))
            (req^.env & userId .~ user)
      withNoAuthRespond = runReaderT (runAppM (handler <&>
          responseLBS (createStatus @code) defaultHeaders . encode))
            (req^.env)
      defaultHeaders = [("Content-type", "application/json")]

instance ( KnownMethod m, KnownNat code
          ) => HasServer (Verb m code 'Raw BL.ByteString) where
  type Server (Verb m code 'Raw BL.ByteString) =
    AppM (Environment Handler) Handler BL.ByteString
  route :: AppM (Environment Handler) Handler BL.ByteString
        -> RoutingInfo -> Maybe (Handler Response)
  route _ (view method -> m) | m /= methodVal @m = Nothing
  route handler req@(view auth -> authLevel) = Just do
    runReaderT (authenticate authLevel (req^.headers)) (req^.env.pool)
      >>= maybe withNoAuthRespond withAuthRespond
    where
      withAuthRespond user =
        runReaderT (runAppM (handler <&>
          responseLBS (createStatus @code) defaultHeaders))
            (req^.env & userId .~ user)
      withNoAuthRespond = runReaderT (runAppM (handler <&>
          responseLBS (createStatus @code) defaultHeaders))
            (req^.env)
      defaultHeaders = [("Content-type", "application/json")]

instance (KnownSymbol s, FromHttpApiData a, HasServer r) =>
    HasServer (QueryParam s a :> r) where
  type Server (QueryParam s a :> r) = Maybe a -> Server r
  route :: (Maybe a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of
  -- because of we need byteString instead of text!
        Left err -> Just $ throwError $ mkError status400 $
            "Can't parse " <> fromString (symbolVal $ Proxy @s)
                           <> " query parameter"
        Right a  -> route @r (f $ Just a) req
      _ -> route @r (f Nothing) req


instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route :: Server a :<|> Server b -> RoutingInfo -> Maybe (Handler Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type Server ((s :: Symbol) :> r) = Server r
  route :: Server r -> RoutingInfo -> Maybe (Handler Response)
  route h req@(view path -> (x:xs)) | symbolVal (Proxy @s) == T.unpack x =
                                      route @r h (req & path .~ xs)
  route _ _ = Nothing

instance ( FromHttpApiData a, HasServer r
         , KnownSymbol id
         ) => HasServer (Capture id a :> r) where
  type Server (Capture id a :> r) = a -> Server r
  route :: (a -> Server r) -> RoutingInfo -> Maybe (Handler Response)
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> Just $ throwError $
      mkError status400 $ "Can't parse capture "
                       <> fromString (symbolVal $ Proxy @id)
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = Nothing

serve :: forall layout. (?env :: Environment Handler, HasServer layout
                         ) => Server layout -> Application
serve s req respond = do
  bodyStr <- strictRequestBody req
  case parseMethod (requestMethod req) of
    Left err -> respond $
      toResponse (mkError status400
                    "Invalid method: GET, POST, PUT and DELETE allowed")
    Right m  -> do
      let reqInfo = RoutingInfo { _path = pathInfo req
                                , _method = m
                                , _queryStr = queryString req
                                , _headers = requestHeaders req
                                , _body = bodyStr
                                , _auth = NoAuth
                                , _env = ?env
                                }
      case route @layout s reqInfo of
        Nothing -> respond $ toResponse err404
        Just handler -> do
          let safeHandler = catch handler \(e :: SomeException) ->
                return $ toResponse (mkError status500 (fromString (show e)))
          runExceptT (runHandler safeHandler)
            >>= either (respond . toResponse) respond
