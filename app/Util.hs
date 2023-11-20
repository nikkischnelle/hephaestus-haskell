{-# LANGUAGE OverloadedStrings #-}

module Util where

import Components.ArticleContent
import Components.FileBrowser
import Pages.ReaderPage

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Data.Text as T (Text, isPrefixOf, isSuffixOf, pack, unpack)
import Data.Text.Lazy as LT (pack, toStrict)
import Network.Wai.Parse (FileInfo (..), lbsBackEnd, parseRequestBody)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, renamePath)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>), takeDirectory)
import System.FilePath.Posix (dropExtension)
import System.Posix (getFileStatus)
import System.Posix.Files (isDirectory)
import Text.Blaze.Html (Html)
import Text.Pandoc (ReaderOptions (readerExtensions), runIO, writeHtml5String)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Extensions
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Writers (writeHtml5)
import Web.Scotty
import Web.Scotty (ActionM)
import Web.Scotty.Trans (ActionT)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists path = removeFile path `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = ioError e

dropFirstDirectory :: String -> String
dropFirstDirectory = drop 1 . dropWhile (/= '/') . dropWhile (== '/')

traverseDirectory :: String -> IO [MenuEntry]
traverseDirectory path = do
  (filePaths, dirPaths) <- getDirectoryContents path
  let fileEntries = [fileEntryFromPath x | x <- filePaths]
  dirEntries <- forM dirPaths $ \subPath -> do
    liftIO $ directoryEntryFromPath subPath

  return (fileEntries ++ dirEntries)

dropPrefix :: String -> String -> String
dropPrefix prefix = drop (length prefix)

directoryEntryFromPath :: FilePath -> IO MenuEntry
directoryEntryFromPath path = do
    subEntries <- traverseDirectory path
    return
            DirectoryEntry
            { entryName = takeBaseName path,
                entryPath = dropPrefix "./markdown" path,
                icon = "directory",
                subEntries = subEntries
            }


getDirectoryContents :: String -> IO ([String], [String])
getDirectoryContents path = do
  subPaths <- listDirectory path
  let actualSubPaths = [path </> subPath | subPath <- subPaths]
  partitionM isFile actualSubPaths

fileEntryFromPath :: FilePath -> MenuEntry
fileEntryFromPath path = do
  let extension = takeExtension path
  let subUrl = dropPrefix "./markdown" path
  if extension == ".md"
    then
      FileEntry
        { entryName = takeBaseName path,
          icon = "\xf0b77",
          entryPath = take (length subUrl - 3) subUrl
        }
    else
      FileEntry
        { entryName = takeFileName path,
          icon = "\xf0976",
          entryPath = subUrl
        }

isFile :: FilePath -> IO Bool
isFile path = do
    fileStatus <- getFileStatus path
    return $ not $ isDirectory fileStatus

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x : xs) = do
    test <- p x
    (ys, zs) <- partitionM p xs
    return $ if test then (x : ys, zs) else (ys, x : zs)