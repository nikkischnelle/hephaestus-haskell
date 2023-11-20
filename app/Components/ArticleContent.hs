{-# LANGUAGE OverloadedStrings #-}

module Components.ArticleContent (Article (..), readToArticle) where

import qualified Data.Char as Char
import Data.Text as T ( pack )
import System.FilePath ( takeBaseName )
import Text.Blaze.Html ( Html )
import Text.Pandoc.Error ( handleError )
import Text.Pandoc.Extensions
import Text.Pandoc.Options ( ReaderOptions (readerExtensions), def )
import Text.Pandoc.Readers ( readMarkdown )
import Text.Pandoc.Writers ( writeHtml5 )
import Text.Pandoc ( runIO )

data Article = Article
  { filePath :: FilePath,
    articleTitle :: String,
    articleContent :: Html
  }

readToArticle :: String -> IO Article
readToArticle path = do
  fileContent <- readFile path
  htmlContent <- markdownToHtml fileContent

  return
    Article
      { filePath = path,
        articleTitle = capitalizeFirstLetter $ takeBaseName path,
        articleContent = htmlContent
      }

readerOptions :: ReaderOptions
readerOptions = def {readerExtensions = 
  githubMarkdownExtensions <> extensionsFromList [
    Ext_subscript,
    Ext_superscript,
    Ext_definition_lists
  ]
}

markdownToHtml :: String -> IO Html
markdownToHtml markdownString = do
  let markdownText = T.pack markdownString
  result <- runIO $ do
    markdown <- readMarkdown readerOptions markdownText
    writeHtml5 def markdown
  handleError result

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (head : tail) = Char.toUpper head : Prelude.map Char.toLower tail
capitalizeFirstLetter [] = []