{-# LANGUAGE OverloadedStrings #-}

module Routes.ViewRoutes where

import Components.ArticleContent ( readToArticle )
import Pages.ReaderPage ( createReaderPage )

import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.FilePath ((</>))
import Web.Scotty ( captureParam, get, html, ScottyM )
import Util ( dropFirstDirectory, traverseDirectory )
import Routes.Patterns ( viewRawFilePattern, viewMarkdownPattern )
import Pages.FileViewPage (createFileViewPage, createImageViewPage, createTextViewPage)
import Magic
import Data.List (isPrefixOf)
import Config (Config (..))

addViewRoutes :: Config -> ScottyM ()
addViewRoutes config = do

    get viewRawFilePattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let url = "/files" </> cleanFileName
        let path = fileStorageDir </> cleanFileName

        magic <- liftIO $ magicOpen [MagicMime]
        liftIO $ magicLoadDefault magic
        mime <- liftIO $ magicFile magic path
        
        if "text/" `isPrefixOf` mime then
            do
                page <- liftIO $ createTextViewPage path fileStorageDir
                Web.Scotty.html $ renderHtml page
        else if "image/" `isPrefixOf` mime then 
            do
                page <- liftIO $ createImageViewPage url fileStorageDir
                Web.Scotty.html $ renderHtml page
        else do
            page <- liftIO $ createFileViewPage url fileStorageDir
            Web.Scotty.html $ renderHtml page

    get viewMarkdownPattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let path = fileStorageDir </> cleanFileName ++ ".md"
        do
            page <- liftIO $ createReaderPage path fileStorageDir
            Web.Scotty.html $ renderHtml page

    where 
        fileStorageDir = storagePath config </> "files"
        fileTrashDir = storagePath config </> "trash"