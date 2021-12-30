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


data Handle m = Handle { _lcreate :: Name -> Surname -> Login
                                  -> Maybe Pictures.ID -> Password -> m Users.ID
                       , _lget    :: Users.ID -> m Payload
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _lcreate = \name surname login mPicture passwd ->
      runReaderT (run (DB.create name surname login mPicture passwd)) pl
  , _lget = \uid -> runReaderT (run (DB.get uid)) pl
  }

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
