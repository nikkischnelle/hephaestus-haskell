{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy as LT (pack)

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        Web.Scotty.html "<h1>root</h1>"

    get (regex "^.*\\.(.*)$") $ do
        beam <- captureParam "0"
        file $ mconcat [".", beam]

    get (regex "^.*$") $ do
        beam <- captureParam "0"
        liftIO $ print beam
        do
            fileContent <- liftIO $ readFile (mconcat [".", beam, ".md"])
            Web.Scotty.text $ LT.pack fileContent