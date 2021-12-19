{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Test.Tasty
import Test.Tasty.Hspec
import Universum

import Test.Common

main :: IO ()
main = do
  common <- testSpecs spec_extractToken
  defaultMain $ testGroup "All tests"
                       [testGroup "Common utils" common]
