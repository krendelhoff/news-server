{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE BangPatterns #-}
module Application.Effects.Auth where

import Hasql.Pool         (Pool)
import Network.HTTP.Types
import Universum


import qualified Application.Auth as Auth
import qualified Types.Users      as Users


import Application                   (HasAuth(auth), HasUtils)
import Application.Auth
import Application.Effects.CanReject
import Application.Effects.Utils
import Errors
import Types.Auth
import Types.Environment
import Types.TH
import Types.Users
import Utils                         (extractToken)

import qualified Application.Utils as Utils

class CanReject m => AuthenticateUser m where
  user :: m Users.ID

class CanReject m => AuthenticateAdmin m where
  whoami :: m Users.ID

class Monad m => Auth m where
  login        :: Login -> Password -> m (Maybe LoginInfo)
  issueToken   :: ExpirationDate -> IsAdmin -> (AccessToken, RefreshToken)
               -> Users.ID -> m AuthPayload

class Auth m => AuthenticateWeak m where
  getTokenInfo :: m TokenInfo

instance (Monad m, HasAuth env (Auth.Handle m)
          ) => Auth (AppM env m) where
  login logn passwd  = do
    authHandle <- view auth
    lift $ view llogin authHandle logn passwd
  issueToken exprDate isAdmin (at, rt) uid = do
    authHandle <- view auth
    lift $ view lissueToken authHandle exprDate isAdmin (at, rt) uid


instance (Monad m, HasAuth env (Auth.Handle m)
          , HasAuthInfo m, UsesCurrentTime (AppM env m)
           , CanReject (AppM env m)
            ) => AuthenticateUser (AppM env m) where
  user = do
    lift ask >>= \case
      Nothing -> reject err401
      Just someByteStr -> case extractToken someByteStr of
        Left NoToken -> reject err401
        Left BadToken -> reject err403TokenInvalid
        Right token -> do
          authHandle <- view auth
          let getTokenInfo tkn curTime = lift $
                view lgetTokenInfo authHandle tkn curTime
          getCurrentTime >>= getTokenInfo token >>= \case
            Just (TokenInfo _ _ (toBool -> True) _ _) ->
              reject err403TokenExpired
            Just (TokenInfo _ _ (toBool -> False) _ user) ->
              return user
            _ -> reject err403

type HasAuthInfo = MonadReader (Maybe ByteString)

instance (Monad m, HasAuth env (Auth.Handle m)
          , HasAuthInfo m, UsesCurrentTime (AppM env m)
           , CanReject (AppM env m)
            ) => AuthenticateAdmin (AppM env m) where
  whoami = do
    lift ask >>= \case
      Nothing -> reject err404
      Just someByteStr -> case extractToken someByteStr of
        Left NoToken -> reject err404
        Left BadToken -> reject err404
        Right token -> do
          authHandle <- view auth
          let getTokenInfo tkn curTime = lift $
                view lgetTokenInfo authHandle tkn curTime
          getCurrentTime >>= getTokenInfo token >>= \case
            Just (TokenInfo _ (toBool -> True) (toBool -> True) _ _) ->
              reject err403TokenExpired
            Just (TokenInfo _ (toBool -> True) (toBool -> False) _ user) ->
              return user
            _ -> reject err404


instance (Monad m, HasAuth env (Auth.Handle m), HasAuthInfo m
          , CanReject (AppM env m), UsesCurrentTime (AppM env m)
           , Auth (AppM env m)
            ) => AuthenticateWeak (AppM env m) where
  getTokenInfo = do
    (!token,!uid) <- who
    curTime <- getCurrentTime
    authHandle <- view auth
    lift (view lgetTokenInfo authHandle token curTime)
    >>= maybe (reject err403) return
    where
      who = do
        lift ask >>= \case
          Nothing -> reject err401
          Just someByteStr -> case extractToken someByteStr of
            Left NoToken -> reject err401
            Left BadToken -> reject err403TokenInvalid
            Right token -> do
              authHandle <- view auth
              let getTokenInfo tkn curTime = lift $
                    view lgetTokenInfo authHandle tkn curTime
              getCurrentTime >>= getTokenInfo token >>= \case
                Just (TokenInfo token _ _ _ user) ->
                  return (token, user)
                _ -> reject err403
