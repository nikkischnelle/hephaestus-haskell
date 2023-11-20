{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href, src, defer, style )
import Text.Blaze.XHtml5.Attributes ( rel )
import Components.ArticleContent (readToArticle, Article (..))
import Components.FileBrowser ( createFileBrowser )
import Text.Blaze.Html5.Attributes ( name, content, onclick )
import Control.Exception (catch)

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: FilePath -> IO H.Html
createReaderPage path = do
    article <- catch (readToArticle path) handler
    generateReaderPage article
    where
        handler :: IOError -> IO Article
        handler _ = return generateNotFoundArticle

generateNotFoundArticle :: Article
generateNotFoundArticle = do
    Article { filePath = "", articleTitle = "Not Found", articleContent = H.h1 "404 - Article not found"}

generateReaderPage :: Article -> IO H.Html
generateReaderPage article = do
    browser <- createFileBrowser "./markdown"
    return $ H.docTypeHtml $ do
        createHeader $ articleTitle article
        H.body $ do
            H.div H.! class_ "row" $ do
                createLightModeToggle
                createColumn "column-1" browser
                createColumn "column-2" $ articleContent article


createColumn :: String -> H.Html -> H.Html
createColumn columnClass = H.div H.! class_ (H.toValue columnClass)


-- Header
createHeader :: String -> H.Html
createHeader title = H.head $ do
        createTitle $ "Hephaestus - " ++ title
        createLink "stylesheet" "/webresources/stylesheets/styles.css"
        createScript "/webresources/script.js"
        createMeta "viewport" "width=device-width, initial-scale=1.0"

createTitle :: String -> H.Html
createTitle title = H.title $ H.toHtml title

createLink :: String -> String -> H.Html
createLink relValue hrefValue = H.link H.! rel (H.toValue relValue) H.! href (H.toValue hrefValue)

createScript :: String -> H.Html
createScript srcValue = H.script H.! src (H.toValue srcValue) $ ""

createMeta :: String -> String -> H.Html
createMeta nameValue contentValue = H.meta H.! name (H.toValue nameValue) H.! content (H.toValue contentValue)


-- Light Mode Toggle
createButton :: String -> String -> String -> H.Html
createButton buttonClass onclickValue iconClass = 
    H.button H.! class_ (H.toValue buttonClass) H.! onclick (H.toValue onclickValue) $ do
        H.i H.! class_ (H.toValue iconClass) $ ""

createLightModeToggle :: H.Html
createLightModeToggle = createButton "lightmode-toggle" "toggleLightMode()" "nf nf-md-theme_light_dark"