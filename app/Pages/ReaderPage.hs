{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Control.Monad (forM_)
import Text.Blaze.XHtml5.Attributes (rel, onload)
import Data.List (isPrefixOf)
import Components.ArticleContent (Article, articleTitle, articleContent)
import Components.FileBrowser
import Text.Blaze.Html5.Attributes (name)
import Text.Blaze.Html5.Attributes (content, onclick)

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Article -> [MenuEntry] -> Html
createReaderPage article list = docTypeHtml $ do
    H.head $ do
        H.title $ toHtml $ articleTitle article
        H.link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
        H.link ! rel "stylesheet" ! href "/stylesheets/styles.css"
        H.script ! src "/script.js" $ ""
        H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        -- H.style " .material-symbols-outlined { font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24}"
    H.body $ do
        H.div ! class_ "row" $ do
            H.button ! class_ "lightmode-toggle" ! onclick "toggleLightMode()" $ do
                H.i ! class_ "nf nf-md-theme_light_dark" $ ""
            H.div ! class_ "column-1" $ browser list
            H.div ! class_ "column-2" $ do
                articleContent article