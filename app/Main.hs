{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty ( get, middleware, redirect, scotty )

import Routes.FileRoutes ( addFileRoutes )
import Routes.ResourceRoutes ( addResourcesRoutes )
import Routes.TrashRoutes ( addTrashRoutes )
import Routes.ViewRoutes ( addViewRoutes )
import Routes.Patterns ()
import Middleware ( logStdout, limitRequestSize )


main :: IO ()
main = scotty 3000 $ do
    middleware logStdout
    middleware limitRequestSize

    get "/" $ do
        Web.Scotty.redirect "/view/main"

    addViewRoutes
    addFileRoutes
    addResourcesRoutes
    addTrashRoutes