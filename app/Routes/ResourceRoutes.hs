{-# LANGUAGE OverloadedStrings #-}

module Routes.ResourceRoutes where

import System.FilePath ((</>))
import Web.Scotty ( captureParam, file, get, ScottyM )
import Util ( dropFirstDirectory )
import Routes.Patterns ( webResourcesPattern )

addResourcesRoutes :: ScottyM ()
addResourcesRoutes = do
    get webResourcesPattern $ do
        parameter <- captureParam "0"
        let cleanFileName = dropFirstDirectory parameter
        file $ "./web" </> cleanFileName