{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        Web.Scotty.html "<h1>root</h1>"

    get (regex "^.*\\.(.*)$") $ do
        beam <- captureParam "0"
        file $ mconcat [".", beam]