{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Auth where

import Control.Lens.TH      (makeFieldsNoPrefix)
import Control.Monad.Except (throwError)
import Hasql.Pool           (Pool)
import Universum            hiding (Handle)

import Types.Auth
import Types.Users (ExpirationDate, IsAdmin, Login, Password)
import Types.Utils (CurrentTime)
import Infrastructure

import qualified Database.Auth as DB
import qualified Types.Users   as Users


data Handle m = Handle { _login :: Login -> Password -> m (Maybe LoginInfo)
                       , _issueToken :: ExpirationDate -> IsAdmin
                                     -> (AccessToken, RefreshToken)
                                     -> Users.ID -> m AuthPayload
                       , _getTokenInfo :: AccessToken -> CurrentTime
                                        -> m (Maybe TokenInfo)
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _login = \logn passwd -> runReaderT (run (DB.login logn passwd)) pl
  , _issueToken = \exprDate isAdmin (at, rt) uid ->
      runReaderT (run (DB.issueToken exprDate isAdmin (at,rt) uid)) pl
  , _getTokenInfo = \t curTime ->
      runReaderT (run (DB.getTokenInfo t curTime)) pl
  }
