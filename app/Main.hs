{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty ( get, middleware, redirect, scotty )

import Routes.FileRoutes ( addFileRoutes )
import Routes.ResourceRoutes ( addResourcesRoutes )
import Routes.TrashRoutes ( addTrashRoutes )
import Routes.ViewRoutes ( addViewRoutes )
import Routes.Patterns ()
import Middleware ( limitRequestSize, fileLogger, stdOutLogger )
import Config ( readConfig, Config (..), LoggingConfig (..) )
import Control.Monad.IO.Class (liftIO)
import System.Directory (removeFile, doesDirectoryExist, createDirectory, listDirectory, createDirectoryIfMissing)

import Util
import Control.Monad ( when, unless, forM_ )
import Data.ByteString.Char8 as CBS (unpack)
import Data.Text.Lazy (pack)
import EmbeddedFiles
import System.FilePath ((</>), takeDirectory)
import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    config <- readConfig
    let loggingConfig = logging config
    deleteFileIfExists $ filePath loggingConfig

    logger <- fileLogger $ filePath loggingConfig
    stdoutlogger <- stdOutLogger

    handleEmptyStorageDir $ storagePath config

    scotty (port config) $ do

        when (logToStdOut loggingConfig) $ middleware stdoutlogger

        middleware logger
        middleware limitRequestSize

        get "/" $ do
            redirect "/view/index"

        addViewRoutes config
        addFileRoutes config
        addResourcesRoutes config
        addTrashRoutes config


handleEmptyStorageDir :: String -> IO ()
handleEmptyStorageDir path = do
    let filesPath = path </> "files"
        trashPath = path </> "trash"

    createDirectoryIfMissing True trashPath
    createDirectoryIfMissing True filesPath

    files <- listDirectory filesPath
    when (null files) $ do
        forM_ defaultRootDir $ \(path, content) -> do
            let filePath = filesPath </> path
            createDirectoryIfMissing True $ takeDirectory filePath
            BS.writeFile filePath content