{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module Server.Admin.Authors where

import Universum hiding (get)

import Effects
import Infrastructure
import Server.Errors     (authorNotFoundError)
import Types.Auth
import Types.Authors     (ID, Payload(..), UpdateForm(UpdateForm))
import Types.Environment (AuthenticatedApp)

import qualified Effects.Authors as Authors
import qualified Types.Users     as Users

type API = "authors" :> (DowngradeAPI :<|> UpdateAPI :<|> GetAPI)

server :: ServerT API (AuthenticatedApp '[Admin, User])
server = downgrade :<|> update
                   :<|> get

type DowngradeAPI = Capture "author_id" ID :> Verb 'DELETE 204 'JSON NoContent

downgrade :: PersistAuthor m => ID -> m NoContent
downgrade = Authors.get >=> \case
  Nothing              -> reject authorNotFoundError
  Just (Payload aid _) -> Authors.downgrade aid

type UpdateAPI = Capture "author_id" ID :> ReqBody 'JSON UpdateForm
                                        :> Put Payload

update :: PersistAuthor m => ID -> UpdateForm -> m Payload
update mAid (UpdateForm desc) = Authors.get mAid >>= \case
  Nothing              -> reject authorNotFoundError
  Just (Payload aid _) -> Authors.update aid desc


type GetAPI = Capture "author_id" ID :> Get Payload

get :: PersistAuthor m => ID -> m Payload
get = Authors.get >=> \case
  Nothing      -> reject authorNotFoundError
  Just payload -> return payload
