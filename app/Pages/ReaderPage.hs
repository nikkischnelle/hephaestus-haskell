{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Control.Monad (forM_)
import Text.Blaze.XHtml5.Attributes (rel, onload)
import Data.List (isPrefixOf)
import Components.ArticleContent (Article, articleTitle, articleContent)
import Components.FileBrowser

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Article -> [MenuEntry] -> Html
createReaderPage article list = docTypeHtml $ do
    H.head $ do
        H.title $ toHtml $ articleTitle article
        H.link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
        H.link ! rel "stylesheet" ! href "/collapselist.css"
        H.script ! src "/script.js" $ ""
        -- H.style " .material-symbols-outlined { font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24}"
    H.body $ do
        browser list
        toHtml $ articleContent article