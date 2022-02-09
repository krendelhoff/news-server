{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server.User.Pictures where

import Universum

import Effects
import Infrastructure
import Types.Auth
import Types.Environment (AuthenticatedApp)
import Types.Pictures

import qualified Effects.Pictures as Pictures

import qualified Data.ByteString.Lazy as BL


server :: ServerT API (AuthenticatedApp '[User])
server = persist

type API = "pictures" :> ReqBody 'Raw BL.ByteString
                      :> Post Payload

persist :: PersistPicture m => BL.ByteString -> m Payload
persist = (Payload <$>) . Pictures.persist
