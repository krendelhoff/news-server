{-# LANGUAGE TypeApplications #-}
module Main where

import Universum
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy.Char8 as BLC

app :: Application
app req respond = respond $ responseLBS status200 [] (BLC.pack . show $ req)

settings :: Settings
settings = setBeforeMainLoop (putStrLn @Text "Server started...") defaultSettings

main :: IO ()
main = runSettings settings app
