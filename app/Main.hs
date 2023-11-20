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
import System.Directory (removeFile)
import Control.Monad (when)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

main :: IO ()
main = do
    config <- readConfig
    let loggingConfig = logging config
    deleteFileIfExists $ filePath loggingConfig

    logger <- fileLogger $ filePath loggingConfig
    stdoutlogger <- stdOutLogger

    scotty (port config) $ do
        when (logToStdOut loggingConfig) $ middleware stdoutlogger
        
        middleware logger
        middleware limitRequestSize
        
        get "/" $ do
            Web.Scotty.redirect "/view/main"

        addViewRoutes
        addFileRoutes
        addResourcesRoutes
        addTrashRoutes

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists path = removeFile path `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = ioError e