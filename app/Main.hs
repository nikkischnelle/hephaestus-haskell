{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text as T ( Text, pack, unpack, isSuffixOf, isPrefixOf )
import Data.Text.Lazy as LT (pack, toStrict)
-- import Text.Blaze.Html5.Attributes as A
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

import Pages.ReaderPage
import Text.Blaze.Html (Html)

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
        file $ mconcat ["./markdown", beam]

    get (regex "^.*$") $ do
        beam <- captureParam "0"
        liftIO $ print beam
        do
            fileContent <- liftIO $ readFile (mconcat ["./markdown", beam, ".md"])

            dirContents <- liftIO $ traverseDir "./markdown" (T.isPrefixOf "./markdown/." . T.pack)

            let markdownList = [dropPrefix "./markdown" $ dropExtension x | x <- dirContents, T.isSuffixOf ".md" $ T.pack x]
                entryList = [MenuEntry {
                    name = x,
                    path = x,
                    icon = "directory",
                    subEntries = []
                } | x <- markdownList]

            h <- liftIO $ mdToHtml $ T.pack fileContent

            let page = createReaderPage h entryList
            Web.Scotty.html $ LT.pack $ renderHtml page

dropPrefix :: String -> String -> String
dropPrefix prefix = drop (length prefix)

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

mdToHtml :: Text -> IO Html
mdToHtml markdown = do
  result <- runIO $ do
    markdown <- readMarkdown readerOptions markdown
    writeHtml5 def markdown
  handleError result