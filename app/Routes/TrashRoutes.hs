{-# LANGUAGE OverloadedStrings #-}

module Routes.TrashRoutes where

import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, removeFile, renamePath)
import System.FilePath ((</>), takeDirectory)
import Web.Scotty ( delete, captureParam, post, ScottyM, text )
import Util ( dropFirstDirectory )
import Routes.Patterns ( fileInTrashPattern )

addTrashRoutes :: ScottyM ()
addTrashRoutes = do
    delete fileInTrashPattern $ do
        parameter <- captureParam "0"
        let path = dropFirstDirectory parameter
        let filepath = "./trash" </> path
        liftIO $ print filepath
        liftIO $ removeFile filepath
        text "File deleted successfully."

    post fileInTrashPattern $ do
        parameter <- captureParam "0"
        let path = dropFirstDirectory parameter
        let trashPath = "./trash" </> path
        let storagePage = "./markdown" </> path
        liftIO $ createDirectoryIfMissing True $ takeDirectory storagePage
        liftIO $ renamePath trashPath storagePage
        text "File restored successfully."