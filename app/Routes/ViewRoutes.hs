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

addViewRoutes :: ScottyM ()
addViewRoutes = do

    get viewRawFilePattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let url = "/files" </> cleanFileName
        let path = "./markdown" </> cleanFileName

        magic <- liftIO $ magicOpen [MagicMime]
        liftIO $ magicLoadDefault magic
        mime <- liftIO $ magicFile magic path
        
        if "text/" `isPrefixOf` mime then
            do
                page <- liftIO $ createTextViewPage path
                Web.Scotty.html $ renderHtml page
        else if "image/" `isPrefixOf` mime then 
            do
                page <- liftIO $ createImageViewPage url
                Web.Scotty.html $ renderHtml page
        else do
            page <- liftIO $ createFileViewPage url
            Web.Scotty.html $ renderHtml page

    get viewMarkdownPattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let path = "./markdown" </> cleanFileName ++ ".md"
        do
            -- entryList <- liftIO $ traverseDirectory "./markdown"
            -- article <- liftIO $ readToArticle path
            page <- liftIO $ createReaderPage path
            Web.Scotty.html $ renderHtml page