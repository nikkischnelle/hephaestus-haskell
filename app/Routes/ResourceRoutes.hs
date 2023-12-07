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
import Web.Scotty.Trans (setHeader, ActionT)
import Config (Config (..))
import Data.Text.Internal.Lazy as InternalLazyT (Text)

addResourcesRoutes :: Config -> ScottyM ()
addResourcesRoutes config = do
    -- returns css files
    -- i.e. getting localhost:{port}/webresources/styles.css returns the embedded styles.css
    get cssPattern $ do
        parameter <- captureParam "0"
        getEmbeddedFileAndSetContentType "text/css" parameter
    
    -- returns javascript files
    -- i.e. getting localhost:{port}/webresources/script.js returns the embedded script.js
    get javascriptPattern $ do
        parameter <- captureParam "0"
        getEmbeddedFileAndSetContentType "text/javascript" parameter


-- | helper function that gets a file at the selected location and 
-- sets the respone's content type header to the one specified
getEmbeddedFileAndSetContentType :: InternalLazyT.Text -> String -> ActionT IO ()
getEmbeddedFileAndSetContentType contentType fileName = do
    let cleanFileName = dropFirstDirectory fileName
    case getWebResource cleanFileName of
        Just fileContent -> do
            setHeader "Content-Type" contentType
            text $ T.pack $ unpack fileContent
        Nothing -> text "Resource not found"