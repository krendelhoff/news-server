{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server.Pictures where

import Universum

import Application.Effects
import Infrastructure
import Types.Environment
import Types.Pictures

import qualified Application.Effects.Pictures as Pictures

import qualified Data.ByteString.Lazy as BL

server :: Server API
server = persist

type API = "pictures" :> ReqBody 'Raw BL.ByteString
         :> Post Payload

persist :: PersistPicture m => BL.ByteString -> m Payload
persist = (Payload <$>) . Pictures.persist
