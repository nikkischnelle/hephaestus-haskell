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


createReaderPage :: FilePath -> IO H.Html
createReaderPage path = do
    fileBrowser <- createFileBrowser "./markdown"
    article <- catch (readToArticle path) noArticleFoundError
    return $ generateViewPage (articleTitle article) fileBrowser (articleContent article)

noArticleFoundError :: IOError -> IO Article
noArticleFoundError _ = return generateNotFoundArticle

generateNotFoundArticle :: Article
generateNotFoundArticle = do
    Article { filePath = "", articleTitle = "Not Found", articleContent = H.h1 "404 - Article not found"}