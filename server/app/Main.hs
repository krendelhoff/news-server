{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Concurrent  (forkIO)
import Data.Yaml           (ParseException, decodeFileThrow)
import Hasql.Pool          (Pool)
import Network.Wai         (Application)
import Options.Applicative
import Universum           hiding (toText)

import qualified Network.Wai.Handler.Warp as Warp

import Infrastructure
import Logger
import Migration         (applyMigrations)
import Server            (API, server)
import Server.Auth       (authenticate)
import Types.Environment
import Utils             (parseToken, (=>>))
import Data.Coerce       (coerce)

main :: IO ()
main = do
  configFilePath <- execParser opts
  conf           <- readConfigurationFile configFilePath
  logger         <- launchLogger conf
  withPool (conf^.dbConfig) (conf^.dbPoolSettings) \pool -> do
    applyMigrations pool
    let env = Environment pool conf logger
        warpSettings = mkWarpSettings (conf^.port)
        servingHandle = mkServingHandle pool logger
    Warp.runSettings warpSettings (mkApp servingHandle env)

opts :: ParserInfo FilePath
opts = info parsers description
  where
    configFilePathParser :: Parser FilePath
    configFilePathParser = strOption ( long "config"
                                    <> short 'c'
                                    <> metavar "FILE"
                                    <> help "Configuration file"
                                    <> value "config.yaml"
                                     )
    parsers = configFilePathParser <**> helper
    description = fullDesc <> header "The News Server"

readConfigurationFile :: FilePath -> IO Config
readConfigurationFile configFilePath =
  decodeFileThrow @IO @Config configFilePath `catch` \(e :: ParseException) ->
    do putStrLn @Text "Configuration file format violation"
       failGracefully (SomeException e)

launchLogger :: Config -> IO Logger
launchLogger conf = newLogger =>> forkIO . mkLoggingThread loggerConf
  where loggerConf = if conf^.doLogging
                     then Logging (conf^.logLevel)
                     else NoLogging

mkWarpSettings :: Port -> Warp.Settings
mkWarpSettings portNumber = Warp.setBeforeMainLoop action settings
  where
    message = "Server initialized at localhost:" <> show portNumber
    action = putStrLn @Text message
    settings =
      Warp.setPort (fromIntegral . coerce @Port @Word16 $ portNumber) Warp.defaultSettings

mkServingHandle :: Pool -> Logger -> ServingHandle
mkServingHandle pool logger = ServingHandle
  { _extractToken = (toText <$>) . parseToken
  , _authenticate = flip runReaderT pool . authenticate
  , _log          = flip runReaderT logger . log ERROR
  }

mkApp :: ServingHandle -> Environment -> Application
mkApp handle env = serve @API handle (unlift @API (runApp env) server)
