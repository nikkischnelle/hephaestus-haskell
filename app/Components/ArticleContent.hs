{-# LANGUAGE OverloadedStrings #-}

module Components.ArticleContent (Article, readToArticle, articleTitle, articleContent) where

import Text.Blaze.Html (Html)
import Text.Pandoc (runIO)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Options (ReaderOptions, def)
import Text.Pandoc (ReaderOptions(readerExtensions))
import Text.Pandoc.Extensions
import Text.Pandoc.Writers (writeHtml5)
import Text.Pandoc.Error (handleError)
import System.FilePath (takeBaseName)
import Data.Text as T
import qualified Data.Char as Char

data Article = Article {
    filePath :: FilePath,
    articleTitle :: String,
    articleContent :: Html
}

readToArticle :: String -> IO Article
readToArticle path = do
    let filePath = mconcat ["./markdown", path, ".md"] :: FilePath
    fileContent <- readFile filePath
    htmlContent <- markdownToHtml fileContent

    return Article {
        filePath = filePath,
        articleTitle = capitalizeFirstLetter $ takeBaseName path,
        articleContent = htmlContent
    }

readerOptions :: ReaderOptions
readerOptions = def {
    readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_pipe_tables, Ext_table_captions]
}

markdownToHtml :: String -> IO Html
markdownToHtml markdownString = do
    let markdownText = T.pack markdownString
    result <- runIO $ do
        markdown <- readMarkdown readerOptions markdownText
        writeHtml5 def markdown
    handleError result

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (head:tail) = Char.toUpper head : Prelude.map Char.toLower tail
capitalizeFirstLetter [] = []