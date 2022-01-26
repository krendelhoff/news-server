{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Utils where

import Control.Lens.TH (makeFieldsNoPrefix)
import Universum       hiding (Handle)


import Types.Utils (CurrentTime)

import qualified Utils

data Handle m = Handle { _genRandomBytes :: Int -> m ByteString
                       , _getCurrentTime :: m CurrentTime
                       }
makeFieldsNoPrefix ''Handle

new :: MonadIO m => IO (Handle m)
new = return $ Handle
  { _genRandomBytes = Utils.getRandomBytes
  , _getCurrentTime = Utils.getCurrentTime
  }
