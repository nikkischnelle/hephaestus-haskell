{-# LANGUAGE OverloadedStrings #-}

module Routes.Patterns where

import Web.Scotty ( regex, RoutePattern )

-- | matches every route that begins with /trash
fileInTrashPattern :: RoutePattern
fileInTrashPattern = regex "^/trash/(.*)$"

-- | matches every route that begins with /webresources and ending in .css
cssPattern :: RoutePattern
cssPattern = regex "^/webresources/.*\\.(css)$"

-- | matches every route that begins with /webresources and ending in .js
javascriptPattern :: RoutePattern
javascriptPattern = regex "^/webresources/.*\\.(js)$"

-- | matches every route that begins with /files.
-- i.e. /files/test.md
filePattern :: RoutePattern
filePattern = regex "^/files/(.*)$"

-- | matches every route that begins with /view and has a . inside it.
-- i.e. /view/test.md
viewRawFilePattern :: RoutePattern
viewRawFilePattern = regex "^/view/(.*)\\.(.*)$"

-- | matches every route that begins with /view.
--  i.e. /view/test (but also /view/test.md)
viewMarkdownPattern :: RoutePattern
viewMarkdownPattern = regex "^/view/(.*)$"