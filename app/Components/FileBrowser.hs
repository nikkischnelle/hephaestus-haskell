{-# LANGUAGE OverloadedStrings #-}

module Components.FileBrowser where

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, style, onclick)
import Control.Monad (forM_)
import Data.List (isPrefixOf)

browser :: [MenuEntry] -> Html
browser list = H.div ! class_ "filebrowser" $ do
    H.ul $ forM_ list listFromDirectory


listFromDirectory :: MenuEntry -> Html
listFromDirectory entry = case entry of
    DirectoryEntry _ icon name subEntries -> H.li $ do
            button ! class_ "collapsible browserButton" $ do
                H.span ! class_ "material-icons" $ "folder"
                toHtml name
            H.ul ! class_ "collapsible_content" $ do
                forM_ subEntries $ listFromDirectory

    FileEntry path fileIcon name -> H.li $ do
            H.button ! class_ "fileButton browserButton" ! onclick (toValue $ "redirect('" ++ path ++ "')") $ do
                H.span ! class_ "material-icons" $ toHtml fileIcon
                toHtml name
            H.i ! class_ (toValue fileIcon) $ ""


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