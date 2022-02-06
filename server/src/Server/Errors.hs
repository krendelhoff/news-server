{-# LANGUAGE OverloadedStrings #-}
module Server.Errors where

import Errors         (ServerError, mkError)
import Infrastructure (status403, status404)
import Universum

categoryNotFoundError :: ServerError
categoryNotFoundError = mkError status404 "Category not found"

authorNotFoundError :: ServerError
authorNotFoundError = mkError status403 "Author not found"
