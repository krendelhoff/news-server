module Common where

import Universum

import qualified Data.Time as Time

import Types.Auth
import Types.TH

getCurrentTime :: MonadIO m => m CurrentTime
getCurrentTime = liftIO Time.getCurrentTime <&> fromUTCTime
