{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
module Migration where

import Database.Persist.Migration
import Database.Persist.PersistValue
import Universum

import Migration.Initial

-- TODO write some TH to reduce boilerplate

migration :: Migration
migration = [ 0 ~> 1 := [ addUUIDExtension
                        , createUsers
                        , createAuth
                        , createAuthors
                        , createCategories
                        , createCategoriesContent
                        , createTags
                        , createNews
                        , createPostPhotos
                        , createPostTags
                        , createComments
                        ]
            ]
