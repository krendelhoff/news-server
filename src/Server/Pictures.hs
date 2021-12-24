{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Server.Pictures where

import Universum

import Common
import Types.Common
import Types.Router
import Types.Users

import qualified Data.ByteString.Lazy as BL

type API = "pictures" :> ReqBody 'Raw BL.ByteString
         :> Verb 'POST 200 'Raw NoContent
