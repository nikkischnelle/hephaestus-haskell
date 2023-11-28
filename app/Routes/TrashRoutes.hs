{-# LANGUAGE OverloadedStrings #-}

module Routes.TrashRoutes where

import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, removeFile, renamePath)
import System.FilePath ((</>), takeDirectory)
import Web.Scotty ( delete, captureParam, post, ScottyM, text )
import Util ( dropFirstDirectory )
import Routes.Patterns ( fileInTrashPattern )
import Config (Config (..))

addTrashRoutes :: Config -> ScottyM ()
addTrashRoutes config = do
    delete fileInTrashPattern $ do
        parameter <- captureParam "0"
        let path = dropFirstDirectory parameter
        let filepath = fileTrashDir </> path
        liftIO $ removeFile filepath
        text "File deleted successfully."

    post fileInTrashPattern $ do
        parameter <- captureParam "0"
        let path = dropFirstDirectory parameter
        let trashPath = fileTrashDir </> path
        let storagePage = fileStorageDir </> path
        liftIO $ createDirectoryIfMissing True $ takeDirectory storagePage
        liftIO $ renamePath trashPath storagePage
        text "File restored successfully."

    where 
        fileStorageDir = storagePath config </> "files"
        fileTrashDir = storagePath config </> "trash"