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
    return $ generateViewPage "File Viewer" fileBrowser subPage

createImageViewPage :: FilePath -> IO H.Html
createImageViewPage path = do
    fileBrowser <- createFileBrowser "./markdown"
    let image = H.img H.! src (H.toValue path) H.! class_ "fileViewImageViewer"
    return $ generateViewPage "Image Viewer" fileBrowser image

createTextViewPage :: FilePath -> IO H.Html
createTextViewPage path = do
    fileBrowser <- createFileBrowser "./markdown"
    fileContent <- readFile path
    let image = H.pre H.! class_ "textViewer" $ H.toHtml fileContent
    return $ generateViewPage "Text Viewer" fileBrowser image