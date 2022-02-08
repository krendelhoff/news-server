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
import Types.Utils
import Types.Categories
import Types.Environment (AuthenticatedApp)

import qualified Effects.Categories as Categories

type API = "categories" :> (GetAPI :<|> CreateAPI)

server :: ServerT API (AuthenticatedApp '[User])
server = get :<|> create


type GetAPI = QueryParam "recursive" Bool :> Capture "category_id" ID
                                          :> Get (SumType Payload PayloadRecursive)

get :: AcquireCategory m => Maybe Bool -> ID -> m (SumType Payload PayloadRecursive)
get (Just True) =
  Categories.getRecursive
  >=> maybe (reject categoryNotFoundError) (return . SumB)
get _ =
  Categories.get
  >=> maybe (reject categoryNotFoundError) (return . SumA)


type CreateAPI = "create" :> ReqBody 'JSON CreateForm
                          :> Post Payload

create :: PersistCategory m => CreateForm -> m Payload
create (CreateForm title mParent) =
  Categories.create title mParent >>= \case
    Nothing  -> reject (mkError status403 "Category name is not unique")
    Just cat -> return cat
