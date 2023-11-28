{-# LANGUAGE OverloadedStrings #-}

module Routes.ResourceRoutes where

import System.FilePath ((</>))
import Web.Scotty ( captureParam, file, get, ScottyM, text )
import Util ( dropFirstDirectory )
import Routes.Patterns ( cssPattern, javascriptPattern )
import EmbeddedFiles (getWebResource)
import Data.ByteString.Char8 (unpack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text.Lazy as T (pack)
import Web.Scotty.Trans (setHeader)
import Config (Config (..))


addResourcesRoutes :: Config -> ScottyM ()
addResourcesRoutes config = do
    get cssPattern $ do
        parameter <- captureParam "0"
        getEmbeddedFileAndSetContentType "text/css" parameter
    
    get javascriptPattern $ do
        parameter <- captureParam "0"
        getEmbeddedFileAndSetContentType "text/javascript" parameter

getEmbeddedFileAndSetContentType contentType fileName = do
    let cleanFileName = dropFirstDirectory fileName
    case getWebResource cleanFileName of
        Just fileContent -> do
            setHeader "Content-Type" contentType
            text $ T.pack $ unpack fileContent
        Nothing -> text "Resource not found"