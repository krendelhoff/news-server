{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Server.User.Categories where

import Universum hiding (get, toText)

import Effects
import Infrastructure
import Server.Errors     (categoryNotFoundError)
import Types.Auth
import Types.Categories
import Types.Environment (AuthenticatedApp)

import qualified Effects.Categories as Categories

type API = "categories" :> (GetAPI :<|> CreateAPI)

server :: ServerT API (AuthenticatedApp '[User])
server = get :<|> create


type GetAPI = Capture "category_id" ID :> Get Payload

get :: AcquireCategory m => ID -> m Payload
get = Categories.get >=> maybe (reject categoryNotFoundError) return


type CreateAPI = "create" :> ReqBody 'JSON CreateForm
                          :> Post Payload

create :: PersistCategory m => CreateForm -> m Payload
create (CreateForm title mParent) =
  Categories.create title mParent >>= \case
    Nothing  -> reject (mkError status403 "Category name is not unique")
    Just cat -> return cat
