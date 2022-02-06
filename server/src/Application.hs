{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application where

import Control.Lens.TH (makeFieldsNoPrefix)
import Hasql.Pool      (Pool)
import Universum       hiding (Handle)

import Types.Logger (Logger)

import qualified Application.Authors    as Authors
import qualified Application.Categories as Categories
import qualified Application.Logging    as Logging
import qualified Application.Pictures   as Pictures
import qualified Application.Users      as Users
import qualified Application.Utils      as Utils

data Handle m = Handle { _persistUser       :: Users.Handle m
                       , _persistPicture    :: Pictures.Handle m
                       , _logging           :: Logging.Handle m
                       , _utils             :: Utils.Handle m
                       , _persistAuthor     :: Authors.Handle m
                       , _persistCategories :: Categories.Handle m
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Logger -> Pool -> IO (Handle m)
new logger pl = do
  _persistUser       <- Users.new pl
  _persistPicture    <- Pictures.new pl
  _logging           <- Logging.new logger
  _utils             <- Utils.new
  _persistAuthor     <- Authors.new pl
  _persistCategories <- Categories.new pl
  return Handle{..}
