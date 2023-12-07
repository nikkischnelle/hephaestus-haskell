{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Text.Blaze.XHtml5.Attributes ( rel )
import Components.ArticleContent (readToArticle, Article (..))
import Components.FileBrowser ( createFileBrowser )
import Text.Blaze.Html5.Attributes ( name, content, onclick )
import Control.Exception (catch)
import Pages.ViewPage (generateViewPage)

-- | creates a reader page for markdown files including skeleton page.
-- Arguments: path to markdown file, path to file browser root directory
createReaderPage :: FilePath -> FilePath -> IO H.Html
createReaderPage articlePath browserBaseDir = do
    fileBrowser <- createFileBrowser browserBaseDir
    article <- catch (readToArticle articlePath) noArticleFoundError
    return $ generateViewPage (articleTitle article) fileBrowser (articleContent article)

-- | is called by createReaderPage if there is no file at the given filepath
-- returns a styled 404 page
noArticleFoundError :: IOError -> IO Article
noArticleFoundError _ = return notFoundPage
    where
        notFoundPage = Article { filePath = "", articleTitle = "Not Found", articleContent = H.h1 "404 - Article not found"}