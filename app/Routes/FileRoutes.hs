{-# LANGUAGE OverloadedStrings #-}

module Routes.FileRoutes where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Network.Wai.Parse (FileInfo (..), lbsBackEnd, parseRequestBody)
import System.Directory (createDirectoryIfMissing, renamePath)
import System.FilePath ((</>), takeDirectory)
import Web.Scotty
import Util
import Routes.Patterns ( filePattern )
import Config (Config (..))

addFileRoutes :: Config -> ScottyM ()
addFileRoutes config = do
    get filePattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        file $ fileStorageDir </> cleanFileName

    post filePattern $ do
        parameter <- captureParam "0"
        let cleanedPath = dropFirstDirectory parameter
        fs <- files
        liftIO $ forM_ fs $ \(fieldName, fileInfo) -> do
            let storageDirectory = fileStorageDir </> cleanedPath
            let storageFilePath = storageDirectory </> BSC.unpack (fileName fileInfo)
            createDirectoryIfMissing True storageDirectory
            BL.writeFile storageFilePath (fileContent fileInfo)
        text "Files uploaded successfully."
    
    delete filePattern $ do
        parameter <- captureParam "0"

        let path = dropFirstDirectory parameter
        let storagePath = fileStorageDir </> path
        let trashPath = fileTrashDir </> path

        liftIO $ createDirectoryIfMissing True $ takeDirectory trashPath
        liftIO $ renamePath storagePath trashPath
        text "File moved to bin successfully."

    where 
        fileStorageDir = storagePath config </> "files"
        fileTrashDir = storagePath config </> "trash"