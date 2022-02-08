{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Server.Admin.Categories where

import Universum hiding (get, toText)

import Effects
import Infrastructure
import Server.Errors
import Types.Auth
import Types.Categories
import Types.Environment (AuthenticatedApp)

import qualified Effects.Categories     as Categories
import qualified Server.User.Categories as UserServer

type API = "categories" :> (RemoveAPI :<|> UpdateAPI)

server :: ServerT API (AuthenticatedApp '[Admin, User])
server = remove :<|> update

type RemoveAPI = "remove" :> Capture "category_id" ID :> Delete NoContent

remove :: PersistCategory m => ID -> m NoContent
remove = Categories.remove

type UpdateAPI = "update" :> Capture "category_id" ID
                          :> QueryParam "title" Title
                          :> QueryParam "parent" ID
                          :> Put Payload

-- FIXME add validation
update :: PersistCategory m => ID -> Maybe Title -> Maybe ID -> m Payload
update mCat mT mP = Categories.get mCat >>= \case
  Nothing                -> reject categoryNotFoundError
  Just (Payload cat _ _) -> update' cat mT mP
  where
    update' :: PersistCategory m => ID -> Maybe Title -> Maybe ID -> m Payload
    update' cat Nothing Nothing = UserServer.get cat
    update' cat Nothing (Just mParent) = Categories.get mParent >>= \case
      Nothing -> reject rebaseDestinationNotExist
      Just (Payload parent _ _) -> Categories.rebase cat parent >>= \case
        Nothing      -> reject incorrectRebaseDestination
        Just payload -> return payload
    update' cat (Just title) Nothing = Categories.rename cat title >>= \case
      Nothing      -> reject titleIsNotUnique
      Just payload -> return payload
    update' cat (Just title) (Just mParent) = do
      update' cat Nothing (Just mParent)
      update' cat (Just title) Nothing
