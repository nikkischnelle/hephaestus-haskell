{-# LANGUAGE OverloadedStrings #-}

module Util where

import Components.ArticleContent

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, removeFile, doesFileExist, doesDirectoryExist)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>))
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

-- | a data type that represents a file or directory in the filebrowser menu.
-- Can either be a FileEntry or a DirectoryEntry.
data MenuEntry = FileEntry {
    entryPath :: String,
    icon :: String,
    entryName :: String
} | DirectoryEntry {
    entryPath :: String,
    icon :: String,
    entryName :: String,
    subEntries :: [MenuEntry]
} deriving (Show, Eq)

-- | deletes a file if it exists
deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists path = removeFile path `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = ioError e

-- | drops the first directory from a FilePath
dropFirstDirectory :: FilePath -> FilePath
dropFirstDirectory = drop 1 . dropWhile (/= '/') . dropWhile (== '/')

-- | drops the first n directories from a FilePath
-- Arguments: FilePath, number of directories to drop
dropNDirectories :: FilePath -> Integer -> FilePath
dropNDirectories path 0 = path
dropNDirectories path n = dropNDirectories (dropFirstDirectory path) (n-1)

-- | traverses a directory and returns a list of all subdirectories and files (recursively)
-- Arguments: FilePath to directory
-- Returns: List of MenuEntries, one for each file or directory in the directory
-- subdirectories are subEntries of DirectoryEntries
traverseDirectory :: FilePath -> IO [MenuEntry]
traverseDirectory path = do
  (filePaths, dirPaths) <- getDirectoryContents path
  let fileEntries = [fileEntryFromPath x | x <- filePaths]
  dirEntries <- forM dirPaths $ \subPath -> do
    liftIO $ directoryEntryFromPath subPath

  return (fileEntries ++ dirEntries)

-- | creates a single DirectoryEntry for a given path.
-- The DirectoryEntry will have a list of subEntries that are the MenuEntries of the files and directories
-- Arguments: FilePath to directory.
directoryEntryFromPath :: FilePath -> IO MenuEntry
directoryEntryFromPath path = do
    subEntries <- traverseDirectory path
    return
            DirectoryEntry
            { entryName = takeBaseName path,
                entryPath = dropNDirectories path 2,
                icon = "directory",
                subEntries = subEntries
            }

-- | gets directory contents and returns a tuple of lists of files and directories
-- is used internally to show files first and then directories in the filebrowser
getDirectoryContents :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryContents path = do
  subPaths <- listDirectory path
  let actualSubPaths = [path </> subPath | subPath <- subPaths]
  -- doesFileExists returns false for directories, true for files
  partitionM doesFileExist actualSubPaths

-- | creates a fileentry from a path
-- Arguments: FilePath to file
-- Distinguishes between markdown files and other files for the icon
fileEntryFromPath :: FilePath -> MenuEntry
fileEntryFromPath path = do
  let extension = takeExtension path
  let subUrl = dropNDirectories path 2
  if extension == ".md"
    then
      FileEntry
        { entryName = takeBaseName path,
          icon = "nf nf-cod-file",
          entryPath = "/view" </> take (length subUrl - 3) subUrl -- drop the '.md'
        }
    else
      FileEntry
        { entryName = takeFileName path,
          icon = "nf nf-md-image",
          entryPath = "/view" </> subUrl
        }

-- | partitions a list into two lists based on a predicate
-- Arguments: predicate, list to partition
-- Returns: tuple of two lists, first list contains all elements that satisfy the predicate
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x : xs) = do
    test <- p x
    (ys, zs) <- partitionM p xs
    return $ if test then (x : ys, zs) else (ys, x : zs)