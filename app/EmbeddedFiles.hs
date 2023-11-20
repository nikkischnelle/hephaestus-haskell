{-# LANGUAGE TemplateHaskell #-}

module EmbeddedFiles where

import qualified Data.ByteString
import Data.FileEmbed (embedDir, embedFile)

embeddedWebResources :: [(FilePath, Data.ByteString.ByteString)]
embeddedWebResources = $(embedDir "embeddedFiles/web")

getWebResource :: FilePath -> Maybe Data.ByteString.ByteString
getWebResource path = lookup path embeddedWebResources

introPage :: Data.ByteString.ByteString
introPage = $(embedFile "embeddedFiles/index.md")