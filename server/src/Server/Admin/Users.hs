{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}
module Server.Admin.Users where

import Universum hiding (get)
import Control.Lens.Unsound (lensProduct)

import Effects
import Infrastructure
import Server.Errors        (cantRemoveAdminError, userNotFoundError)
import Types.Auth
import Types.Environment    (AuthenticatedApp)
import Types.Users

import qualified Effects.Users as Users

type API = "users" :> DeleteAPI

server :: ServerT API (AuthenticatedApp '[Admin, User])
server = delete

type DeleteAPI = Capture "user_id" ID :> Delete NoContent

userInfo :: Lens' Payload (ID, IsAdmin)
userInfo = lensProduct userId privileged

delete :: ManageUser m => ID -> m NoContent
delete mUid = Users.getById mUid >>= \case
  Nothing                                     -> reject userNotFoundError
  Just (view userInfo -> (_, toBool -> True)) -> reject cantRemoveAdminError
  Just (view userInfo -> (uid, _           )) -> NoContent <$ Users.delete uid
