{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TypeApplications #-}
module Server.Admin.Authors where

import Data.Coerce (coerce)
import Universum   hiding (get)

import Effects
import Infrastructure
import Server.Errors     (authorNotFoundError, userNotFoundError, authorAlreadyExistsError)
import Types.Auth
import Types.Authors     (ID(ID), Payload(..), UpdateForm(UpdateForm), Description)
import Types.Environment (AuthenticatedApp)

import qualified Effects.Authors as Authors
import qualified Effects.Users   as Users
import qualified Types.Users     as Users
import Types.Users (HasUserId(userId))

type API = "authors" :> (DowngradeAPI :<|> UpdateAPI
                                      :<|> GetAPI
                                      :<|> PromoteAPI)

server :: ServerT API (AuthenticatedApp '[Admin, User])
server = downgrade :<|> update
                   :<|> get
                   :<|> promote

type PromoteAPI = ReqBody 'JSON Payload :> Post Payload

promote :: (ManageUser m, PersistAuthor m
            ) => Payload -> m Payload
promote (Payload (coerce -> mUid) desc) = Authors.get (coerce mUid) >>= \case
  Nothing -> Authors.promote mUid desc >>= \case
    Nothing -> reject userNotFoundError
    Just p  -> return p
  _       -> reject authorAlreadyExistsError

type DowngradeAPI = Capture "author_id" ID :> Verb 'DELETE 204 'JSON NoContent

downgrade :: PersistAuthor m => ID -> m NoContent
downgrade = Authors.get >=> \case
  Nothing                   -> reject authorNotFoundError
  Just (view userId -> aid) -> NoContent <$ Authors.downgrade aid


type UpdateAPI = Capture "author_id" ID :> ReqBody 'JSON UpdateForm
                                        :> Put Payload

update :: PersistAuthor m => ID -> UpdateForm -> m Payload
update aid (UpdateForm desc) = Authors.update aid desc >>= \case
  Nothing -> reject authorNotFoundError
  Just p  -> return p

type GetAPI = Capture "author_id" ID :> Get Payload

get :: PersistAuthor m => ID -> m Payload
get = Authors.get >=> \case
  Nothing      -> reject authorNotFoundError
  Just payload -> return payload
