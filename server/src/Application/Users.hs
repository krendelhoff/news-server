{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Users where

import Control.Lens.TH (makeFieldsNoPrefix)
import Hasql.Pool      (Pool)
import Universum       hiding (Handle, get)

import Types.Users (Login, Name, Password, Payload, Surname)
import Infrastructure

import qualified Database.Users as DB
import qualified Types.Pictures as Pictures
import qualified Types.Users    as Users


data Handle m = Handle { _create :: Name -> Surname -> Login
                                 -> Maybe Pictures.ID -> Password -> m Users.ID
                       , _get    :: Users.ID -> m Payload
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _create = \name surname login mPicture passwd ->
      runReaderT (run (DB.create name surname login mPicture passwd)) pl
  , _get = \uid -> runReaderT (run (DB.get uid)) pl
  }
