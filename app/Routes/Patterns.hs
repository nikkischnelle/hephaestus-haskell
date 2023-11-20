{-# LANGUAGE OverloadedStrings #-}

module Routes.Patterns where

import Web.Scotty ( regex, RoutePattern )

fileInTrashPattern :: RoutePattern
fileInTrashPattern = regex "^/trash/(.*)$"

webResourcesPattern :: RoutePattern
webResourcesPattern = regex "^/webresources/.*\\.(js|css)$"

filePattern :: RoutePattern
filePattern = regex "^/files/(.*)$"

viewPattern :: RoutePattern
viewPattern = regex "^/view/(.*)$"