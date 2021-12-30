{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Authors where

import Universum hiding (get)

import Application.Effects
import Infrastructure
import Types.Authors       (ID, Payload, UpdateForm(UpdateForm))
import Types.Environment   (Server)

import qualified Application.Effects.Authors as Authors
import qualified Types.Users                 as Users

type API = "authors" :> (DowngradeAPI :<|> UpdateAPI :<|> GetAPI)

server :: Server API
server = downgrade :<|> update
                   :<|> get

type DowngradeAPI = "downgrade" :> Capture "author_id" ID
                                :> Verb 'PUT 204 'JSON NoContent

downgrade :: PersistAuthor m => ID -> m NoContent
downgrade = Authors.downgrade

type UpdateAPI = "update" :> Capture "author_id" ID
                          :> ReqBody 'JSON UpdateForm
                          :> Post Payload

update :: PersistAuthor m => ID -> UpdateForm -> m Payload
update aid (UpdateForm desc) = Authors.update aid desc

type GetAPI = Capture "author_id" ID :> Get Payload

get :: PersistAuthor m => ID -> m Payload
get = Authors.get >=> \case
  Nothing      -> reject (mkError status403 "Author not found")
  Just payload -> return payload
