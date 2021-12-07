{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import Hasql.Connection          hiding (settings)
import Network.HTTP.Types.Status
import Network.Wai
import Universum

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

import DB
import Migration         (applyMigrations)
import Router
import Types.DB
import Types.Environment

serverSettings :: Warp.Settings
serverSettings = Warp.setBeforeMainLoop
                   (putStrLn @Text "Server initialized at localhost:5435...")
                   Warp.defaultSettings

type MyAPI = "boba" :> "biba" :> Get Int

app :: Environment -> Server MyAPI
app = runReaderT (return 5)

-- parse from dhall
-- settings function
connStr :: ConnectionString
connStr = "host=localhost port=5435 user=savely dbname=db password="

poolSettings :: Pool.Settings
poolSettings = (10, 5, connStr)

main :: IO ()
main = do -- TODO Cont
  withPool poolSettings \pool -> do
    applyMigrations pool
    let conf = DbConfig "localhost" 5435 "savely" "db" ""
        env = Environment pool conf
    Warp.runSettings serverSettings $ serve @MyAPI $ app env
