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

viewRawFilePattern :: RoutePattern
viewRawFilePattern = regex "^/view/(.*)\\.(.*)$"

viewMarkdownPattern :: RoutePattern
viewMarkdownPattern = regex "^/view/(.*)$"