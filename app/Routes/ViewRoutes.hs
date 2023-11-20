{-# LANGUAGE OverloadedStrings #-}

module Routes.ViewRoutes where

import Components.ArticleContent ( readToArticle )
import Pages.ReaderPage ( createReaderPage )

import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.FilePath ((</>))
import Web.Scotty ( captureParam, get, html, ScottyM )
import Util ( dropFirstDirectory, traverseDirectory )
import Routes.Patterns ( viewPattern )

addViewRoutes :: ScottyM ()
addViewRoutes = do
    get viewPattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        let path = "./markdown" </> cleanFileName ++ ".md"
        do
            liftIO $ print path
            entryList <- liftIO $ traverseDirectory "./markdown"
            article <- liftIO $ readToArticle path
            let page = createReaderPage article entryList
            Web.Scotty.html $ renderHtml page


