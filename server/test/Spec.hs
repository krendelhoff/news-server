{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Test.Tasty
import Test.Tasty.Hspec
import Universum

import Test.Utils
import Test.User.Categories
import Test.User.Pictures
import Test.User.Users
import Test.Admin.Categories
import Test.Admin.Authors


spec = do
  spec_authors
  spec_admin_categories
  spec_user_categories
  spec_users
  spec_pictures

main :: IO ()
main = do
  utils <- testSpecs spec_utils
  apiSpec <- testSpecs spec
  --users <- testSpecs spec_create
  defaultMain $
    testGroup "All tests" [-- testGroup "Database/Users" users
                            testGroup "Common utils" utils
                          , testGroup "API" apiSpec
                          ]
