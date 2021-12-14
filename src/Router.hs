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

import Common
import Database.Auth
import Types.Auth
import Types.Common
import Types.Environment
import Types.Router
import Types.TH
import Types.Users

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Text            as T

-- type instance Server (QueryParams s a :> r) = Vector a -> Server r


class HasServer layout where
  type Server (layout :: Type) :: Type
  route :: Server layout -> RequestInfo -> Handler Response

extractToken :: RequestHeaders -> Either TokenError Token
extractToken hMap = case lookup "Authorization" hMap of
  Nothing -> Left NoToken
  Just (((fromText <$>) . decodeUtf8' <$>)
       . B.break isSpace -> ("Bearer", Right token)) -> Right token
  _ -> Left BadToken

instance HasServer r => HasServer (RequireAdmin :> r) where
  type Server (RequireAdmin :> r) = Server r
  route :: Server r -> RequestInfo -> Handler Response
  route handler req = route @r handler (req & auth .~ True)

instance ( KnownMethod m, KnownNat code, ToJSON a
         ) => HasServer (Verb m code a) where
  type Server (Verb m code a) = Handler a
  route :: Handler a -> RequestInfo -> Handler Response
  route _ (view method -> m) | m /= methodVal @m = throwError WrongPath
  route handler req@(view headers -> hMap) | req^.auth =
    case extractToken hMap of
      Right token -> do
        getCurrentTime >>= run . getTokenInfo token >>= \case
          Just (TokenInfo _ (toBool -> True) (toBool -> True) _) ->
            throwError err401TokenExpired
          Just (TokenInfo user (toBool -> True) (toBool -> False) _) ->
            local (set userId user) handler <&>
              responseLBS (mkStatus (fromInteger
                (natVal (Proxy @code))) "Success") [] . encode
          _ -> throwError err404
      _ -> throwError err404
  route handler req@(view headers -> hMap) =
    case extractToken hMap of
      Left NoToken -> throwError err401
      Left BadToken -> throwError err401TokenInvalid
      Right token -> do
        getCurrentTime >>= run . getTokenInfo token >>= \case
          Just (TokenInfo _ _ (toBool -> True) _) ->
            throwError err401TokenExpired
          Just (TokenInfo user _ (toBool -> False) _) ->
            local (set userId user) handler <&>
              responseLBS (mkStatus (fromInteger
                (natVal (Proxy @code))) "Success") [] . encode
          _ -> throwError err404


instance ( FromJSON a, HasServer r ) => HasServer (ReqBody a :> r) where
  type Server (ReqBody a :> r) = a -> Server r
  route :: (a -> Server r) -> RequestInfo -> Handler Response
  route f req@(view body -> bodyStr) = case decodeStrict bodyStr of
    Nothing -> throwError $ mkError status400 "Can't parse request body"
    Just a  -> route @r (f a) req

instance (KnownSymbol s, FromHttpApiData a, HasServer r) =>
    HasServer (QueryParam s a :> r) where
  type Server (QueryParam s a :> r) = Maybe a -> Server r
  route :: (Maybe a -> Server r) -> RequestInfo -> Handler Response
  route f req@(view queryStr -> params) =
    case lookup (encodeUtf8 (symbolVal (Proxy @s))) params of
      Just (Just x) -> case parseHeader x of
  -- because of we need byteString instead of text!
        Left err -> throwError $ mkError status400 $
            "Can't parse " <> fromString (symbolVal $ Proxy @s)
                           <> " query parameter"
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
  route _ _ = throwError WrongPath

instance ( FromHttpApiData a, HasServer r
         , KnownSymbol id
         ) => HasServer (Capture id a :> r) where
  type Server (Capture id a :> r) = a -> Server r
  route :: (a -> Server r) -> RequestInfo -> Handler Response
  route handler req@(view path -> (x:xs)) = case parseUrlPiece x of
    Left err -> throwError $
      mkError status400 $ "Can't parse capture "
                       <> fromString (symbolVal $ Proxy @id)
    Right a  -> route @r (handler a) (req & path .~ xs)
  route _ _ = throwError WrongPath

toResponse :: ServerError -> Response
toResponse WrongPath          = toResponse err404
toResponse (ServerError st m) = responseLBS st [] (encode m)

serve :: forall layout. (?env :: Environment, HasServer layout) => Server layout
                                                                -> Application
serve s req respond = do
  bodyStr <- BL.toStrict <$> strictRequestBody req
  case parseMethod (requestMethod req) of
    Left err -> respond $
      toResponse (mkError status400
                    "Invalid method: GET, POST, PUT and DELETE allowed")
    Right m  -> do
      let reqInfo = RequestInfo { _path = pathInfo req
                                , _method = m
                                , _queryStr = queryString req
                                , _headers = requestHeaders req
                                , _body = bodyStr
                                , _auth = False
                                }
      runExceptT (runReaderT (runHandler do route @layout s reqInfo) ?env)
      >>= either (respond . toResponse) respond
