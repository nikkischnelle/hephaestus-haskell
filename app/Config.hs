{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Config (Config (..), readConfig, LoggingConfig (..)) where

import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Control.Exception ( catch )
import System.IO.Error

data Config = Config {
    port :: Int,
    logging :: LoggingConfig,
    storagePath :: FilePath
} deriving (Eq, Show)

data LoggingConfig = LoggingConfig {
    filePath :: FilePath,
    logToStdOut :: Bool
} deriving (Eq, Show)

instance Y.FromJSON Config where
    parseJSON :: Y.Value -> Y.Parser Config
    parseJSON = Y.withObject "Config" $ \obj -> do
        port <- obj Y..: "port"
        storagePath <- obj Y..: "storagePath"
        loggingConf <- obj Y..: "logging"
        logPath <- loggingConf Y..: "logPath"
        logToStdOut <- loggingConf Y..: "logToStdOut"
        return Config { 
            port = port, 
            logging = LoggingConfig {
                filePath = logPath,
                logToStdOut = logToStdOut
            },
            storagePath = storagePath
        }

instance Y.ToJSON Config where
    toJSON :: Config -> Y.Value
    toJSON config = Y.object [
        "port" Y..= port config,
        "logging" Y..= logging config,
        "storagePath" Y..= storagePath config
        ]

instance Y.ToJSON LoggingConfig where
    toJSON :: LoggingConfig -> Y.Value
    toJSON loggingConf = Y.object [
        "logPath" Y..= filePath loggingConf,
        "logToStdOut" Y..= logToStdOut loggingConf]

defaultConfig :: Config
defaultConfig = Config {
    port = 3000,
    logging = defaultLoggingConfig,
    storagePath = "./`markdown`"
}

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig {
    filePath = "hephaestus.log",
    logToStdOut = False
}

readConfig :: IO Config
readConfig = do
    putStrLn "Reading config..."
    fileContent <- catch (B.readFile "config.yaml") handleFileNotFound
    config <- case Y.decodeEither' fileContent of
        Left err -> handleReadConfigError err
        Right config -> return config
    return (config :: Config)

handleReadConfigError :: Y.ParseException -> IO Config
handleReadConfigError err = do
    putStrLn $ "Error parsing config: " ++ show err
    putStrLn "Please review your config.yaml file and try again."
    return defaultConfig

handleFileNotFound :: IOError -> IO B.ByteString
handleFileNotFound e = do
          putStrLn $ "Error reading config file: " ++ show e
          putStrLn "Using default config and storing it to config.new.yaml; please review and move to config.yaml."
          catch (B.writeFile "config.new.yaml" $ Y.encode defaultConfig) handleWriteConfigError
          return $ Y.encode defaultConfig

handleWriteConfigError :: IOError -> IO ()
handleWriteConfigError e = do
          putStrLn $ "Error writing config.new.yaml file: " ++ show e
          return ()