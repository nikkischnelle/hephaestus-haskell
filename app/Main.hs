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

    -- Delete old log file if it exists already
    deleteFileIfExists $ filePath loggingConfig
    -- Create logger for file
    logger <- fileLogger $ filePath loggingConfig
    -- Always create logger for stdout
    stdOutLogger <- stdOutLogger

    -- Create storage directory if it doesn't exist
    handleEmptyStorageDir $ storagePath config

    scotty (port config) $ do
        -- Only enable stdout logger if enabled in config
        when (logToStdOut loggingConfig) $ middleware stdOutLogger

        middleware logger
        middleware limitRequestSize

        -- Redirect root to index view
        get "/" $ do
            redirect "/view/index"

        -- Add routes
        addViewRoutes config
        addFileRoutes config
        addResourcesRoutes config
        addTrashRoutes config


-- | If the storage dir does not exist or is empty, creates it and adds default files 
-- (copied from ./embeddedFiles/defaultRoot)
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
            -- create missing directories from defaultRoot in storage
            createDirectoryIfMissing True $ takeDirectory filePath
            -- write file from defaultRoot to storage
            BS.writeFile filePath content