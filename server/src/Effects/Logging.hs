module Effects.Logging where

import Universum

import Types.Environment (AuthenticatedApp)
import Types.Logger      (Level)

import qualified Logger

class Monad m => Logging m where
  log :: Level -> Text -> m ()

instance (
          ) => Logging (AuthenticatedApp rights) where
  log = Logger.log
