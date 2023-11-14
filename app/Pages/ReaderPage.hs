{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src )
import Control.Monad (forM_)
import Text.Blaze.XHtml5.Attributes (rel)

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Html -> [MenuEntry] -> Html
createReaderPage text list = docTypeHtml $ do
    H.head $ do
        H.title "Hephaestus"
        H.link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
    H.body $ do
        explorer list
        toHtml text

explorer :: [MenuEntry] -> Html
explorer list = H.div ! class_ "explorer" $ do
    H.ul ! class_ "collapse" $ forM_ list listFromDirectory

listFromDirectory :: MenuEntry -> Html
listFromDirectory entries = case entries of
    DirectoryEntry path icon name subEntries -> do
        li $ do
            a $ toHtml name
            ul $ forM_ subEntries listFromDirectory
    
    FileEntry path fileIcon name -> li $ do
        let test = "test" :: [Char]
        H.span ! class_ "material-icons" $ toHtml fileIcon
        a ! href (toValue path) $ toHtml name
        i ! class_ (toValue fileIcon) $ ""


data MenuEntry = FileEntry {
    path :: String,
    icon :: String,
    name :: String
} | DirectoryEntry {
    path :: String,
    icon :: String,
    name :: String,
    subEntries :: [MenuEntry]
} deriving (Show)