{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Test.Tasty
import Test.Tasty.Hspec
import Universum

import Test.Utils

import qualified Test.User.Categories as User.Categories
import qualified Test.User.Pictures as User.Pictures
import qualified Test.User.Users as User.Users
import qualified Test.Admin.Categories as Admin.Categories
import qualified Test.Admin.Authors as Admin.Authors
import qualified Test.Admin.Users as Admin.Users


spec :: Spec
spec = do
  Admin.Authors.spec
  Admin.Categories.spec
  Admin.Users.spec
  User.Categories.spec
  User.Users.spec
  User.Pictures.spec

main :: IO ()
main = do
  utils   <- testSpecs spec_utils
  apiSpec <- testSpecs spec
  defaultMain $
    testGroup "All tests" [ testGroup "Common utils" utils
                          , testGroup "API" apiSpec
                          ]
