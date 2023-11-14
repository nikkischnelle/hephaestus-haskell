{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text as T ( Text, pack, unpack, isSuffixOf, isPrefixOf )
import Data.Text.Lazy as LT (pack, toStrict)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Pandoc (runIO)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Writers (writeHtml5)
import Text.Pandoc.Options (def)
import Text.Pandoc.Error (handleError)
import Text.Pandoc (ReaderOptions)
import Text.Pandoc (ReaderOptions(readerExtensions))
import Text.Pandoc.Extensions
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_pipe_tables, Ext_table_captions]
}

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        Web.Scotty.html "<h1>root</h1>"

    get (regex "^.*\\.(.*)$") $ do
        beam <- captureParam "0"
        file $ mconcat [".", beam]

    get (regex "^.*$") $ do
        beam <- captureParam "0"
        liftIO $ print beam
        do
            fileContent <- liftIO $ readFile (mconcat [".", beam, ".md"])

            h <- liftIO $ mdToHtml $ T.pack fileContent
            
            let page = createPage h
            Web.Scotty.html $ LT.pack $ renderHtml page

createPage :: Html -> Html
createPage text = docTypeHtml $ do
  H.head $ do
    H.title "Hephaestus"
  H.body $ do
    toHtml text

mdToHtml :: Text -> IO Html
mdToHtml markdown = do
  result <- runIO $ do
    markdown <- readMarkdown readerOptions markdown
    writeHtml5 def markdown
  handleError result