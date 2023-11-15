{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO, MonadIO)
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
import System.FilePath ((</>), takeExtension, takeBaseName, takeFileName)
import Control.Monad (forM)
import System.Posix (getFileStatus)
import System.Posix.Files (isDirectory)
import System.FilePath.Posix (dropExtension)
import Data.Foldable (forM_)

import Pages.ReaderPage
import Text.Blaze.Html (Html)
import Web.Scotty.Trans (ActionT)
import Web.Scotty (ActionM)
import Components.ArticleContent (readToArticle)
import Components.FileBrowser

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_pipe_tables, Ext_table_captions]
}

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
      Web.Scotty.redirect "/index"

    get (regex "^.*\\.(js|css)$") $ do
      fileName <- captureParam "0"
      file  $ mconcat ["./web", fileName]

    get (regex "^.*\\.(.*)$") $ do
        beam <- captureParam "0"
        file $ mconcat ["./markdown", beam]

    get (regex "^.*$") $ do
        beam <- captureParam "0"
        liftIO $ print beam
        do
            entryList <- liftIO $ traverseDirectory "./markdown"
            article <- liftIO $ readToArticle beam
            
            let page = createReaderPage article entryList

            Web.Scotty.html $ LT.pack $ renderHtml page


traverseDirectory :: String -> IO [MenuEntry]
traverseDirectory path = do
  (filePaths, dirPaths) <- getDirectoryContents path
  let fileEntries = [fileEntryFromPath x | x <- filePaths]
  dirEntries <- forM dirPaths $ \subPath -> do
    liftIO $ directoryEntryFromPath subPath

  return (fileEntries ++ dirEntries)

getDirectoryContents :: String -> IO ([String], [String])
getDirectoryContents path = do
  subPaths <- listDirectory path
  let actualSubPaths = [path </> subPath | subPath <- subPaths]
  partitionM isFile actualSubPaths

fileEntryFromPath :: FilePath -> MenuEntry
fileEntryFromPath path = do
  let extension = takeExtension path
  let subUrl = dropPrefix "./markdown" path
  if extension == ".md" then
    FileEntry {
      entryName = takeBaseName path,
      icon = "edit",
      entryPath = take (length subUrl - 3) subUrl
    }
  else 
    FileEntry {
      entryName = takeFileName path,
      icon = "image",
      entryPath = subUrl
    }


directoryEntryFromPath :: FilePath -> IO MenuEntry
directoryEntryFromPath path = do
  subEntries <- traverseDirectory path
  return DirectoryEntry {
    entryName = takeBaseName path,
    entryPath = dropPrefix "./markdown" path,
    icon = "directory",
    subEntries = subEntries
  }

dropPrefix :: String -> String -> String
dropPrefix prefix = drop (length prefix)

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