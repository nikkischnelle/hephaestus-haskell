{-# LANGUAGE OverloadedStrings #-}

module Components.FileBrowser where

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, onclick, href)
import Control.Monad (forM_)
import Util
    ( MenuEntry(FileEntry, DirectoryEntry), traverseDirectory )

-- | Creates the file browser
-- Arugments: FilePath to root directory for the browser
createFileBrowser :: FilePath -> IO Html
createFileBrowser path = do
    entryList <- traverseDirectory path
    return $ genereateFileBrowser entryList

-- | generates the File Browser when given a list of MenuEntries
genereateFileBrowser :: [MenuEntry] -> Html
genereateFileBrowser list = H.div ! class_ "filebrowser" $ do
    H.ul ! class_ "filebrowser-list" $ forM_ list listFromDirectory

-- | generates an html list for a menu entry.
-- If the entry is a directory this function is called recursively
listFromDirectory :: MenuEntry -> Html
listFromDirectory entry = case entry of
    DirectoryEntry _ icon name subEntries -> H.li $ do
            button ! class_ "collapsible browserButton" $ do
                H.i ! class_ "nf nf-oct-plus icons" $ ""
                H.i ! class_ "nf nf-cod-folder icons" $ ""
                H.span ! class_ "browser_button_text" $ toHtml name
            H.ul ! class_ "collapsible_content" $ do
                forM_ subEntries listFromDirectory

    FileEntry path fileIcon name -> H.li $ do
        H.a ! href (toValue path) $
            H.button ! class_ "fileButton browserButton" $ do
                H.i ! class_ (toValue $ fileIcon ++ " icons") $ ""
                H.span ! class_ "browser_button_text" $ toHtml name