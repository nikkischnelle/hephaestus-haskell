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
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM)
import System.Posix (getFileStatus)
import System.Posix.Files (isDirectory)
import System.FilePath.Posix (dropExtension)
import Data.Foldable (forM_)

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

            dirContents <- liftIO $ traverseDir "." (T.isPrefixOf "./." . T.pack)

            let markdownList = [drop 1 $ dropExtension x | x <- dirContents, T.isSuffixOf ".md" $ T.pack x]

            h <- liftIO $ mdToHtml $ T.pack fileContent
            
            let page = createPage h markdownList
            Web.Scotty.html $ LT.pack $ renderHtml page

traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top exclude = do
  subPaths <- listDirectory top
  let actualSubPaths = [top </> subPath | subPath <- subPaths]
  (filePaths, dirPaths) <- partitionM isFile actualSubPaths
  paths <- forM (filter (not.exclude) dirPaths) $ \subPath -> do
    let path = subPath
    traverseDir path exclude
  return (filePaths ++ concat paths)

isFile :: FilePath -> IO Bool
isFile path = do
  fileStatus <- getFileStatus path
  return $ not $ isDirectory fileStatus

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  test <- p x
  (ys, zs) <- partitionM p xs
  return $ if test then (x:ys, zs) else (ys, x:zs)

createPage :: Html -> [String] -> Html
createPage text list = docTypeHtml $ do
  H.head $ do
    H.title "Hephaestus"
  H.body $ do
    explorer list
    toHtml text

explorer :: [String] -> Html
explorer list = H.div ! class_ "explorer" $ do
  ul $ forM_ list $ \filepath ->
    li $ a ! href (toValue filepath) $ toHtml filepath

mdToHtml :: Text -> IO Html
mdToHtml markdown = do
  result <- runIO $ do
    markdown <- readMarkdown readerOptions markdown
    writeHtml5 def markdown
  handleError result