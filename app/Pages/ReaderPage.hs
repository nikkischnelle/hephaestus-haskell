{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Text.Blaze.XHtml5.Attributes ( rel )
import Components.ArticleContent (Article, articleTitle, articleContent)
import Components.FileBrowser ( MenuEntry, browser )
import Text.Blaze.Html5.Attributes ( name, content, onclick )

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Article -> [MenuEntry] -> H.Html
createReaderPage article list = H.docTypeHtml $ do        -- H.style " .material-symbols-outlined { font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24}"
    H.head $ do
        H.title $ H.toHtml $ "Hephaestus - " ++ articleTitle article
        H.link H.! rel "stylesheet" H.! href "/webresources/stylesheets/styles.css"
        H.script H.! src "/webresources/script.js" $ ""
        H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1.0"
    H.body $ do
        H.div H.! class_ "row" $ do
            H.button H.! class_ "lightmode-toggle" H.! onclick "toggleLightMode()" $ do
                H.i H.! class_ "nf nf-md-theme_light_dark" $ ""
            H.div H.! class_ "column-1" $ browser list
            H.div H.! class_ "column-2" $ do
                articleContent article