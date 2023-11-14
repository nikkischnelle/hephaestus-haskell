{-# LANGUAGE OverloadedStrings #-}

module Pages.ReaderPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A ( class_, href )
import Control.Monad (forM_)

{- | creates a new reader html page 
takes in the content as html
takes in a list of strings which represent the 
-}
createReaderPage :: Html -> [MenuEntry] -> Html
createReaderPage text list = docTypeHtml $ do
    H.head $ do
        H.title "Hephaestus"
    H.body $ do
        explorer list
        toHtml text

explorer :: [MenuEntry] -> Html
explorer list = H.div ! class_ "explorer" $ do
    H.ul $ forM_ list $ \entry ->
        li ! class_ (toValue $ icon entry) $ a ! href (toValue $ path entry) $ toHtml $ name entry

data MenuEntry = MenuEntry {
    path :: String,
    icon :: String,
    subEntries :: [MenuEntry],
    name :: String
}