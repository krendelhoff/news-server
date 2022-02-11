{-# LANGUAGE OverloadedStrings #-}
module Server.Errors where

import Errors         (ServerError, mkError)
import Infrastructure (status404, status406, status403)

categoryNotFoundError :: ServerError
categoryNotFoundError = mkError status404 ["Category not found"]

authorNotFoundError :: ServerError
authorNotFoundError = mkError status404 ["Author not found"]

authorAlreadyExistsError :: ServerError
authorAlreadyExistsError = mkError status406 ["Current user is already author"]

incorrectRebaseDestination :: ServerError
incorrectRebaseDestination = mkError status406 ["Incorrect rebase destination"]

titleIsNotUnique :: ServerError
titleIsNotUnique = mkError status406 ["New title is not unique"]

rebaseDestinationNotExist :: ServerError
rebaseDestinationNotExist = mkError status406 ["Rebase destination does not exist"]

userNotFoundError :: ServerError
userNotFoundError = mkError status404 ["User not found"]

userAlreadyExistsError :: ServerError
userAlreadyExistsError = mkError status406 ["User already exists"]

cantRemoveAdminError :: ServerError
cantRemoveAdminError = mkError status403 ["Not enough rights to delete admin"]

cantRemoveRootError :: ServerError
cantRemoveRootError = mkError status403 ["Can't remove root category"]

pictureNotExistsError :: ServerError
pictureNotExistsError = mkError status404 ["Picture with given ID does not exist"]
