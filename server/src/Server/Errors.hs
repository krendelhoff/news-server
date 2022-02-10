{-# LANGUAGE OverloadedStrings #-}
module Server.Errors where

import Errors         (ServerError, mkError)
import Infrastructure (status404, status406)

categoryNotFoundError :: ServerError
categoryNotFoundError = mkError status404 ["Category not found"]

authorNotFoundError :: ServerError
authorNotFoundError = mkError status404 ["Author not found"]

incorrectRebaseDestination :: ServerError
incorrectRebaseDestination = mkError status406 ["Incorrent rebase destination"]

titleIsNotUnique :: ServerError
titleIsNotUnique = mkError status406 ["New title is not unique"]

rebaseDestinationNotExist :: ServerError
rebaseDestinationNotExist = mkError status406 ["Rebase destination does not exist"]

userNotFoundError :: ServerError
userNotFoundError = mkError status404 ["User not found"]

userAlreadyExistsError :: ServerError
userAlreadyExistsError = mkError status406 ["User already exists"]
