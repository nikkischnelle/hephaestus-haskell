{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty ( get, middleware, redirect, scotty )

import Routes.FileRoutes ( addFileRoutes )
import Routes.ResourceRoutes ( addResourcesRoutes )
import Routes.TrashRoutes ( addTrashRoutes )
import Routes.ViewRoutes ( addViewRoutes )
import Routes.Patterns ()
import Middleware ( logStdout, limitRequestSize )
import Config ( readConfig, Config (..))


main :: IO ()
main = do
    config <- readConfig

    scotty (port config) $ do
        middleware logStdout
        middleware limitRequestSize

        get "/" $ do
            Web.Scotty.redirect "/view/main"

        addViewRoutes
        addFileRoutes
        addResourcesRoutes
        addTrashRoutes