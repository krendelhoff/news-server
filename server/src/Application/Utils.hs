{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Utils where

import Control.Lens.TH (makeFieldsNoPrefix)
import Universum       hiding (Handle)


import Types.Utils (CurrentTime)

import qualified Utils


data Handle m = Handle { _lgenRandomBytes :: Int -> m ByteString
                       , _lgetCurrentTime :: m CurrentTime
                       }
makeFieldsNoPrefix ''Handle

new :: MonadIO m => IO (Handle m)
new = return $ Handle
  { _lgenRandomBytes = Utils.getRandomBytes
  , _lgetCurrentTime = Utils.getCurrentTime
  }

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => (Handle m -> IO a) -> IO a
withHandle = bracket new close
