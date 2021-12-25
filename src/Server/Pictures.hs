{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server.Pictures where

import Universum

import Application.Effects (PersistPicture)
import Common
import Router
import Types.Common
import Types.Environment
import Types.Router
import Types.Pictures

import qualified Types.Pictures as Pictures
import qualified Application.Effects.Pictures as Pictures

import qualified Data.ByteString.Lazy as BL

server :: Server API
server = persist

type API = "pictures" :> ReqBody 'Raw BL.ByteString
         :> Post Pictures.Payload

persist :: PersistPicture m => BL.ByteString -> m Pictures.Payload
persist = (Payload <$>) . Pictures.persist
