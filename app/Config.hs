{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Config (Config (..), readConfig, LoggingConfig (..)) where

import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Control.Exception ( catch )
import System.IO.Error

data Config = Config {
    port :: Int, -- ^ Port to listen on
    logging :: LoggingConfig, -- ^ Logging configuration
    storagePath :: FilePath -- ^ Path to store files
} deriving (Eq, Show)

data LoggingConfig = LoggingConfig {
    filePath :: FilePath, -- ^ Path to log file; disable logging to file by setting to empty string
    logToStdOut :: Bool -- ^ Whether to log to stdout
} deriving (Eq, Show)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config {
    port = 3000,
    logging = defaultLoggingConfig,
    storagePath = "storage"
}
    where
        defaultLoggingConfig = LoggingConfig {
            filePath = "hephaestus.log",
            logToStdOut = False
        }


instance Y.FromJSON Config where
    -- | Parse a Config from a Yaml object for reading from file
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
    -- | Convert a Config to a Yaml object for writing to file
    toJSON :: Config -> Y.Value
    toJSON config = Y.object [
            "port" Y..= port config,
            "logging" Y..= logging config,
            "storagePath" Y..= storagePath config
        ]

instance Y.ToJSON LoggingConfig where
    -- | Convert a LoggingConfig to a Yaml object for writing to file
    toJSON :: LoggingConfig -> Y.Value
    toJSON loggingConf = Y.object [
        "logPath" Y..= filePath loggingConf,
        "logToStdOut" Y..= logToStdOut loggingConf]

-- | Reads config from config.yaml.
-- If config.yaml does not exist, writes default config to config.new.yaml and returns default config.
readConfig :: IO Config
readConfig = do
    putStrLn "Reading config..."

    -- Calling handleFileNotFound if config.yaml is not accessible
    fileContent <- catch (B.readFile "config.yaml") handleFileNotFound

    -- Calling handleReadConfigError if config.yaml is not valid yaml
    config <- case Y.decodeEither' fileContent of
        Left err -> handleReadConfigError err
        Right config -> return config
    return (config :: Config)


-- | Called by readConfig if config.yaml could not be accessed - presumably because it does not exist.
-- Writes default config to config.new.yaml and returns default config as string for parsing in readConfig.
handleFileNotFound :: IOError -> IO B.ByteString
handleFileNotFound e = do
    putStrLn $ "Error reading config file: " ++ show e
    putStrLn "Using default config and storing it to config.new.yaml; please review and move to config.yaml."

    catch (writeFile "config.new.yaml" defaultConfigYaml) handleWriteConfigError
    return $ Y.encode defaultConfig

    where 
        defaultConfigYaml = "# Port that the webserver listens on. Make sure your user is allowed to use the port\n" ++
                            "port: " ++ show (port defaultConfig) ++ "\n" ++
                            "# Logging configuration\n" ++
                            "logging:\n" ++
                            "  # Path to log file; disable logging to file by setting to empty string\n" ++
                            "  logPath: " ++ show (filePath (logging defaultConfig)) ++ "\n" ++
                            "  # Whether to log to stdout\n" ++
                            "  logToStdOut: " ++ show (logToStdOut (logging defaultConfig)) ++ "\n" ++
                            "# Path to store markdown files in\n" ++
                            "storagePath: " ++ show (storagePath defaultConfig) ++ "\n"

-- | Called by handleFileNotFound if config.new.yaml could not be written to disk.
handleWriteConfigError :: IOError -> IO ()
handleWriteConfigError e = do
          putStrLn $ "Error writing config.new.yaml file: " ++ show e
          return ()

-- | Called by readConfig if config.yaml could not be parsed as yaml.
handleReadConfigError :: Y.ParseException -> IO Config
handleReadConfigError err = do
    putStrLn $ "Error parsing config: " ++ show err
    putStrLn "Please review your config.yaml file and try again."
    putStrLn "Using default config. Remove config.yaml to generate a new config.yaml with default values."
    return defaultConfig