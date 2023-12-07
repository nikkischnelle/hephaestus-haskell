{-# LANGUAGE OverloadedStrings #-}

module Routes.FileRoutes where

import Config (Config (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Network.Wai.Parse (FileInfo (..), lbsBackEnd, parseRequestBody)
import Routes.Patterns (filePattern)
import System.Directory (createDirectoryIfMissing, renamePath)
import System.FilePath (takeDirectory, (</>))
import Util
import Web.Scotty

addFileRoutes :: Config -> ScottyM ()
addFileRoutes config = do

    -- returns the file at the given path
    -- i.e. getting localhost:{port}/files/dir1/test.md will return the file at {storageDir}/files/test.md
    get filePattern $ do
        parameter <- captureParam "0" -- gets the path of the file that is to be sent back
        let cleanFileName = dropFirstDirectory parameter -- remove leading '/files'
        file $ fileStorageDir </> cleanFileName -- respond with the file at the location

    -- uploads file(s) to the selected directory
    -- i.e. posting files "abc.md" and "123.png" to localhost:{port}/files/dir1 will put the in {storageDir}/files/dir1/[abc.md, 123.png]
    post filePattern $ do
        parameter <- captureParam "0" -- gets the directory the file is to be located in from the request
        let cleanedPath = dropFirstDirectory parameter -- remove leading '/files'
        fs <- files -- gets all files from the request
        liftIO $ forM_ fs $ \(fieldName, fileInfo) -> do
            let storageDirectory = fileStorageDir </> cleanedPath
            let storageFilePath = storageDirectory </> BSC.unpack (fileName fileInfo)
            createDirectoryIfMissing True storageDirectory -- create directory for files if missing (also includes parent dirs that don't exist yet)
            BL.writeFile storageFilePath (fileContent fileInfo)
        text "Files uploaded successfully."

    -- moves selected file to trash bin
    -- i.e. deleting localhost:{port}/index.md moves the file at {storageDir}/files/index.md to {storageDir}/trash/index.md
    delete filePattern $ do
        parameter <- captureParam "0" -- gets the path of the file that is to be moved to trash

        let path = dropFirstDirectory parameter -- remove leading '/files'
        let storagePath = fileStorageDir </> path -- construct path to file in storage
        let trashPath = fileTrashDir </> path -- construct path to file in trashbin

        liftIO $ createDirectoryIfMissing True $ takeDirectory trashPath -- create all directories necessary for moving the file
        liftIO $ renamePath storagePath trashPath -- move file to trash bin
        text "File moved to bin successfully."

    where
        fileStorageDir = storagePath config </> "files"
        fileTrashDir = storagePath config </> "trash"