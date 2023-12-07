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

    -- gets an html page that contains the given file and also the 'skeleton page' (which includes the file browser, light/dark mode etc.
    -- i.e. getting localhost:{port}/view/example.pdf shows a page with a pdf reader showing the pdf
    get viewRawFilePattern $ do
        parameter <- captureParam "0" -- gets the path to the file that should be viewed from the request
        let cleanFileName = dropFirstDirectory parameter -- remove leading '/view'
        let url = "/files" </> cleanFileName -- gets the web url to the file itself
        let path = fileStorageDir </> cleanFileName -- gets path to file in storage

        -- get mime type of file
        magic <- liftIO $ magicOpen [MagicMime]
        liftIO $ magicLoadDefault magic
        mime <- liftIO $ magicFile magic path
        
        -- Render text files as text
        if "text/" `isPrefixOf` mime then
            do
                page <- liftIO $ createTextViewPage path fileStorageDir
                Web.Scotty.html $ renderHtml page

        -- Render images as images
        else if "image/" `isPrefixOf` mime then 
            do
                page <- liftIO $ createImageViewPage url fileStorageDir
                Web.Scotty.html $ renderHtml page

        -- Render everything else as a general file, which uses the browsers implementation for showing it
        -- (e.g. pdfs are shown using the pdf viewer of the browser)
        else do
            page <- liftIO $ createFileViewPage url fileStorageDir
            Web.Scotty.html $ renderHtml page

    -- Markdown File Viewer, generates html from markdown using pandoc in background
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