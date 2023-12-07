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

-- | Article Data with path to file, 
-- content of file (formatted as html)
-- and title of the article
data Article = Article
  { 
    filePath :: FilePath,
    articleTitle :: String,
    articleContent :: Html
  }

-- | my reader options for the pandoc markdown reader.
-- Containing Default options but with 
-- Github Markdown Extensions + some extra extensions
readerOptions :: ReaderOptions
readerOptions = def {readerExtensions = 
  githubMarkdownExtensions <> extensionsFromList [
    Ext_subscript, -- extension for subscripting
    Ext_superscript, -- extension for superscripting
    Ext_definition_lists, -- extension for definition lists
    Ext_footnotes -- extensino for footnotes
  ]
}

-- | reads file at path and returns an Article
-- Arguments: path to file
-- Title of Article is set to Name of the file
readToArticle :: String -> IO Article
readToArticle path = do
  fileContent <- readFile path
  htmlContent <- markdownToHtml fileContent readerOptions

  return
    Article
      { 
        filePath = path,
        articleTitle = capitalizeFirstLetter $ takeBaseName path,
        articleContent = htmlContent
      }

-- | converts a string of markdown to html using pandoc
-- Arguments: Markdown String, Markdown Reader Options
markdownToHtml :: String -> ReaderOptions -> IO Html
markdownToHtml markdownString readerOptions = do
  let markdownText = T.pack markdownString
  result <- runIO $ do
    markdown <- readMarkdown readerOptions markdownText
    writeHtml5 def markdown
  handleError result

-- | Capitalizes the first letter of a string
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (head : tail) = Char.toUpper head : Prelude.map Char.toLower tail
capitalizeFirstLetter [] = []