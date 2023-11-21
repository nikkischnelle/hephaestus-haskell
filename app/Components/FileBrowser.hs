{-# LANGUAGE OverloadedStrings #-}

module Components.FileBrowser where

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, onclick)
import Control.Monad (forM_)
import Util


createFileBrowser :: String -> IO Html
createFileBrowser path = do
    entryList <- traverseDirectory path
    return $ genereateFileBrowser entryList

-- generates the File Browser when given a list of MenuEntries
genereateFileBrowser :: [MenuEntry] -> Html
genereateFileBrowser list = H.div ! class_ "filebrowser" $ do
    H.ul ! class_ "filebrowser-list" $ forM_ list listFromDirectory

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
            H.button ! class_ "fileButton browserButton" ! onclick (toValue $ "redirectToView('" ++ path ++ "')") $ do
                H.i ! class_ (toValue $ fileIcon ++ " icons") $ ""
                H.span ! class_ "browser_button_text" $ toHtml name