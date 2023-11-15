{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Control.Monad (forM_)
import Text.Blaze.XHtml5.Attributes (rel, onload)
import Data.List (isPrefixOf)
import Components.Article (Article, articleTitle, articleContent)

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Article -> [MenuEntry] -> String -> Html
createReaderPage article list active = docTypeHtml $ do
    H.head $ do
        H.title $ toHtml $ articleTitle article
        H.link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
        H.link ! rel "stylesheet" ! href "/collapselist.css"
        H.script ! src "/script.js" $ ""
        -- H.style " .material-symbols-outlined { font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24}"
    H.body $ do
        explorer list active
        toHtml $ articleContent article

explorer :: [MenuEntry] -> String -> Html
explorer list active = H.div ! class_ "explorer" $ do
    H.ul $ forM_ list $ listFromDirectory active

listFromDirectory :: String -> MenuEntry -> Html
listFromDirectory active entries = case entries of
    DirectoryEntry _ icon name subEntries -> do
        li $ do
            let entryStyle = if path entries `isPrefixOf` active then "display: block;" else "display: none;" 

            button ! class_ "collapsible" $ do
                H.span ! class_ "material-icons" $ "folder"
                toHtml name
            ul ! class_ "collapsible_content" ! A.style entryStyle $ forM_ subEntries $ listFromDirectory active

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
} | NoEntry deriving (Show, Eq)