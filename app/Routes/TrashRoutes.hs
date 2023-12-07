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

    -- deletes files in trash bin completely
    -- i.e. deleting localhost:{port}/trash/example.md deletes the file at {storageDir}/trash/example.md
    delete fileInTrashPattern $ do
        parameter <- captureParam "0"
        let path = dropFirstDirectory parameter
        let filepath = fileTrashDir </> path
        liftIO $ removeFile filepath
        text "File deleted successfully."

    -- restores files from the trash bin
    -- i.e. posting localhost:{port}/trash/dir1/example.md restores the file at 
    --      {storageDir}/trash/dir1/example.md to {storageDir}/files/dir1/example.md
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