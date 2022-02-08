{-# LANGUAGE OverloadedStrings #-}
module Server.Errors where

import Errors         (ServerError, mkError)
import Infrastructure (status403, status404)
import Universum

categoryNotFoundError :: ServerError
categoryNotFoundError = mkError status404 "Category not found"

authorNotFoundError :: ServerError
authorNotFoundError = mkError status403 "Author not found"

incorrectRebaseDestination :: ServerError
incorrectRebaseDestination = mkError status403 "Incorrent rebase destination"

titleIsNotUnique :: ServerError
titleIsNotUnique = mkError status403 "New title is not unique"

rebaseDestinationNotExist :: ServerError
rebaseDestinationNotExist = mkError status403 "Rebase destination does not exist"
