module Middleware (limitRequestSize, logStdout) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.HTTP.Types as Http
import Data.ByteString.Builder (stringUtf8)

limitRequestSize :: Middleware
limitRequestSize app req respond = do
    case requestBodyLength req of
        KnownLength len -> do
            if len > maxLen
                then respond $ responseBuilder Http.status413 [] (stringUtf8 "Payload too large")
                else app req respond

        ChunkedBody ->
            respond $ responseBuilder Http.status411 [] (stringUtf8 "Length required")
    where
        maxLen = 50*1000*1000 -- 50mB