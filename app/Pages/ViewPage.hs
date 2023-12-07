{-# LANGUAGE OverloadedStrings #-}

module Pages.ViewPage (generateViewPage) where
import Components.ArticleContent
import qualified Text.Blaze.Html5 as H
import Components.FileBrowser (createFileBrowser)
import Text.Blaze.Html5.Attributes

-- | generates a viewing page including two html elements in two columns
-- Arguments: page title, left column element, right column element
-- Basically always used with a filebrowser as input for the left element
generateViewPage :: String -> H.Html -> H.Html -> H.Html
generateViewPage title fileBrowser content = do
    H.docTypeHtml $ do
        createHeader title
        H.body $ do
            H.div H.! class_ "row" $ do
                createLightModeToggle
                createColumn "column-1" fileBrowser
                createColumn "column-2" content

-- | helper class to create a column
createColumn :: String -> H.Html -> H.Html
createColumn columnClass = H.div H.! class_ (H.toValue columnClass)

-- Header
-- | Creates header for the page, injects css and javascript into page, sets viewport and title
-- Argument: Page title
createHeader :: String -> H.Html
createHeader title = H.head $ do
        H.title $ H.toHtml $ "Hephaestus - " ++ title
        addStyleSheet "/webresources/stylesheets/styles.css"
        H.script H.! src "/webresources/script.js" $ ""
        H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1.0"

-- | creates simple html title from string
createTitle :: String -> H.Html
createTitle title = H.title $ H.toHtml title

-- | creates stylesheet references for html
-- Arguments: path to stylesheet
addStyleSheet :: String -> H.Html
addStyleSheet path = H.link H.! rel "stylesheet" H.! href (H.toValue path)

-- | creates script references for html
-- Arguments: path to script
createScript :: String -> H.Html
createScript source = H.script H.! src (H.toValue source) $ ""

-- Light Mode Toggle
-- | creates a button with onclick event handler, class just an icon inside
createIconButton :: String -> String -> String -> H.Html
createIconButton buttonClass onclickValue iconClass = 
    H.button H.! class_ (H.toValue buttonClass) H.! onclick (H.toValue onclickValue) $ do
        H.i H.! class_ (H.toValue iconClass) $ ""

-- | creates the light mode toggle for the page
createLightModeToggle :: H.Html
createLightModeToggle = createIconButton "lightmode-toggle" "toggleLightMode()" "nf nf-md-theme_light_dark"