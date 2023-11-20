{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Config (Config (..), readConfig) where

import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Control.Exception ( catch )
import System.IO.Error

newtype Config
  = Config {port :: Int}
  deriving (Eq, Show)

instance Y.FromJSON Config where
    parseJSON :: Y.Value -> Y.Parser Config
    parseJSON = Y.withObject "Config" $ \obj -> do
        port <- obj Y..: "port"
        return Config { port = port }

instance Y.ToJSON Config where
    toJSON :: Config -> Y.Value
    toJSON config = Y.object ["port" Y..= port config]

defaultConfig :: Config
defaultConfig = Config {
    port = 3000
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