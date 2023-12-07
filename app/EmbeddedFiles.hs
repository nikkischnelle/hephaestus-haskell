{-# LANGUAGE TemplateHaskell #-}

module EmbeddedFiles where

import qualified Data.ByteString
import Data.FileEmbed (embedDir, embedFile)

-- This module embeds multiple files into the executable. They are embedded at compile-time.

embeddedWebResources :: [(FilePath, Data.ByteString.ByteString)]
embeddedWebResources = $(embedDir "embeddedFiles/web")

getWebResource :: FilePath -> Maybe Data.ByteString.ByteString
getWebResource path = lookup path embeddedWebResources

defaultRootDir :: [(FilePath, Data.ByteString.ByteString)]
defaultRootDir = $(embedDir "embeddedFiles/defaultRoot")