{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Server.Categories where

import Universum hiding (get, toText)

import           Application.Effects
import qualified Application.Effects.Categories as Categories
import           Infrastructure
import           Types.Categories
import           Types.Environment

--type API = "categories" :> (GetAPI :<|> CreateAPI
--                                   :<|> RemoveAPI
--                                   :<|> UpdateAPI)
--
--server :: Server API
--server = get :<|> create
--             :<|> remove
--             :<|> update
--
--type GetAPI = Capture "category_id" ID :> Get Payload
--
--notFoundError = mkError status404 "Category not found"
--
--get :: AcquireCategory m => ID -> m Payload
--get = Categories.get >=> maybe (reject notFoundError) return
--
--
--type CreateAPI = "create" :> ReqBody 'JSON CreateForm
--                          :> Post Payload
--
--create :: PersistCategory m => CreateForm -> m Payload
--create (CreateForm title mParent) =
--  Categories.create title mParent >>= \case
--    Nothing  -> reject (mkError status403 "Category name is not unique")
--    Just cat -> return cat
--
--
--type RemoveAPI = "remove" :> Capture "category_id" ID :> Delete NoContent
--
--remove :: PersistCategory m => ID -> m NoContent
--remove = Categories.remove
--
--type UpdateAPI = "update" :> Capture "category_id" ID
--                          :> QueryParam "title" Title
--                          :> QueryParam "parent" ID
--                          :> Put Payload
--
--update :: PersistCategory m => ID -> Maybe Title -> Maybe ID -> m Payload
--update mCat mT mP = Categories.get mCat >>= \case
--  Nothing                -> reject notFoundError
--  Just (Payload cat _ _) -> update' cat mT mP
--  where
--    update' :: PersistCategory m => ID -> Maybe Title -> Maybe ID -> m Payload
--    update' cat Nothing Nothing = get cat
--    update' cat Nothing (Just mParent) = Categories.get mParent >>= \case
--      Nothing -> reject (mkError status403 "Rebase destination does not exist")
--      Just (Payload parent _ _) -> Categories.rebase cat parent >>= \case
--        Nothing      -> reject (mkError status403 "Incorrent rebase destination")
--        Just payload -> return payload
--    update' cat (Just title) Nothing = Categories.rename cat title >>= \case
--      Nothing      -> reject (mkError status403 "New title is not unique")
--      Just payload -> return payload
--    update' cat (Just title) (Just mParent) = do
--      update' cat Nothing (Just mParent)
--      update' cat (Just title) Nothing
