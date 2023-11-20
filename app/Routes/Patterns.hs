{-# LANGUAGE OverloadedStrings #-}

module Routes.Patterns where

import Web.Scotty ( regex, RoutePattern )

fileInTrashPattern :: RoutePattern
fileInTrashPattern = regex "^/trash/(.*)$"

cssPattern :: RoutePattern
cssPattern = regex "^/webresources/.*\\.(css)$"

javascriptPattern :: RoutePattern
javascriptPattern = regex "^/webresources/.*\\.(js)$"

filePattern :: RoutePattern
filePattern = regex "^/files/(.*)$"

viewPattern :: RoutePattern
viewPattern = regex "^/view/(.*)$"