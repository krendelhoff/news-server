{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Test.Tasty
import Test.Tasty.Hspec
import Universum

import Test.Common
import Test.Database.Users

main :: IO ()
main = do
  common <- testSpecs spec_extractToken
  users <- testSpecs spec_create
  defaultMain $ testGroup "All tests"
                       [testGroup "Common utils" common
                       ,testGroup "Database/Users" users]
