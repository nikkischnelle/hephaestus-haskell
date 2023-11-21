{-# LANGUAGE OverloadedStrings #-}

module Pages.ViewPage (generateViewPage) where
import Components.ArticleContent
import qualified Text.Blaze.Html5 as H
import Components.FileBrowser (createFileBrowser)
import Text.Blaze.Html5.Attributes

generateViewPage :: String -> H.Html -> H.Html -> H.Html
generateViewPage title fileBrowser content = do
    H.docTypeHtml $ do
        createHeader title
        H.body $ do
            H.div H.! class_ "row" $ do
                createLightModeToggle
                createColumn "column-1" fileBrowser
                createColumn "column-2" content

createColumn :: String -> H.Html -> H.Html
createColumn columnClass = H.div H.! class_ (H.toValue columnClass)

-- Header
createHeader :: String -> H.Html
createHeader title = H.head $ do
        H.title $ H.toHtml $ "Hephaestus - " ++ title
        addStyleSheet "/webresources/stylesheets/styles.css"
        H.script H.! src "/webresources/script.js" $ ""
        H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1.0"

createTitle :: String -> H.Html
createTitle title = H.title $ H.toHtml title

addStyleSheet :: String -> H.Html
addStyleSheet hrefValue = H.link H.! rel "stylesheet" H.! href (H.toValue hrefValue)

createScript :: String -> H.Html
createScript source = H.script H.! src (H.toValue source) $ ""

-- Light Mode Toggle
createButton :: String -> String -> String -> H.Html
createButton buttonClass onclickValue iconClass = 
    H.button H.! class_ (H.toValue buttonClass) H.! onclick (H.toValue onclickValue) $ do
        H.i H.! class_ (H.toValue iconClass) $ ""

createLightModeToggle :: H.Html
createLightModeToggle = createButton "lightmode-toggle" "toggleLightMode()" "nf nf-md-theme_light_dark"