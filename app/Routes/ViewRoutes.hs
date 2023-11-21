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
import Pages.FileViewPage (createFileViewPage)

addViewRoutes :: ScottyM ()
addViewRoutes = do

    get viewRawFilePattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let path = "/files" </> cleanFileName
        liftIO $ print path
        do
            page <- liftIO $ createFileViewPage path
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