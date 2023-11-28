{-# LANGUAGE OverloadedStrings #-}

module Pages.FileViewPage where

import qualified Text.Blaze.Html5 as H
import Pages.ViewPage (generateViewPage)
import Text.Blaze.Html5.Attributes (src, class_)
import Components.FileBrowser (createFileBrowser)

createFileViewPage :: FilePath -> FilePath -> IO H.Html
createFileViewPage filePath browserBaseDir = do
    fileBrowser <- createFileBrowser browserBaseDir
    let subPage = H.iframe H.! src (H.toValue filePath) H.! class_ "fileViewer" $ ""
    return $ generateViewPage "File Viewer" fileBrowser subPage

createImageViewPage :: FilePath -> FilePath -> IO H.Html
createImageViewPage filePath browserBaseDir = do
    fileBrowser <- createFileBrowser browserBaseDir
    let image = H.img H.! src (H.toValue filePath) H.! class_ "fileViewImageViewer"
    return $ generateViewPage "Image Viewer" fileBrowser image

createTextViewPage :: FilePath -> FilePath -> IO H.Html
createTextViewPage filePath browserBaseDir = do
    fileBrowser <- createFileBrowser browserBaseDir
    fileContent <- readFile filePath
    let image = H.pre H.! class_ "textViewer" $ H.toHtml fileContent
    return $ generateViewPage "Text Viewer" fileBrowser image