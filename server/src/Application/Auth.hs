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



data Handle m = Handle { _llogin :: Login -> Password -> m (Maybe LoginInfo)
                       , _lissueToken :: ExpirationDate -> IsAdmin
                                     -> (AccessToken, RefreshToken)
                                     -> Users.ID -> m AuthPayload
                       , _lgetTokenInfo :: AccessToken -> CurrentTime
                                        -> m (Maybe TokenInfo)
                       }
makeFieldsNoPrefix ''Handle



new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _llogin = \logn passwd -> runReaderT (run (DB.login logn passwd)) pl
  , _lissueToken = \exprDate isAdmin (at, rt) uid ->
      runReaderT (run (DB.issueToken exprDate isAdmin (at,rt) uid)) pl
  , _lgetTokenInfo = \t curTime ->
      runReaderT (run (DB.getTokenInfo t curTime)) pl
  }

close :: (MonadThrow m, MonadIO m) => Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
