{-# LANGUAGE OverloadedStrings #-}

module Pages.FileViewPage where

import qualified Text.Blaze.Html5 as H
import Pages.ViewPage (generateViewPage)
import Text.Blaze.Html5.Attributes (src, class_)
import Components.FileBrowser (createFileBrowser)

createFileViewPage :: FilePath -> IO H.Html
createFileViewPage path = do
    fileBrowser <- createFileBrowser "./markdown"
    let subPage = H.iframe H.! src (H.toValue path) H.! class_ "fileViewer" $ ""
    return $ generateViewPage "FileView" fileBrowser subPage