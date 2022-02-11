{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Server.User.Categories where

import Universum hiding (get, toText)

import Effects
import Infrastructure
import Types.Auth
import Types.Utils
import Server.Errors
import Types.Categories
import Types.Environment (AuthenticatedApp)

import qualified Effects.Categories as Categories

type API = "categories" :> GetAPI

server :: ServerT API (AuthenticatedApp '[User])
server = get

type GetAPI = QueryParam "recursive" Bool :> Capture "category_id" ID
                                          :> Get (SumType Payload PayloadRecursive)

get :: AcquireCategory m => Maybe Bool -> ID -> m (SumType Payload PayloadRecursive)
get (Just True) =
  Categories.getRecursive
  >=> maybe (reject categoryNotFoundError) (return . SumB)
get _ =
  Categories.get
  >=> maybe (reject categoryNotFoundError) (return . SumA)
