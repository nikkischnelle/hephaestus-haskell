module Middleware (limitRequestSize, logStdout, fileLogger, stdOutLogger) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger as Logger
import qualified Network.HTTP.Types as Http
import Data.ByteString.Builder (stringUtf8)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(AppendMode))

fileLogger :: FilePath -> IO Middleware
fileLogger path = do
    handle <- openFile path AppendMode
    let loggerSettings = defaultRequestLoggerSettings { outputFormat = Apache FromSocket , destination = Logger.Handle handle }
    mkRequestLogger loggerSettings

stdOutLogger :: IO Middleware
stdOutLogger = mkRequestLogger defaultRequestLoggerSettings {outputFormat = Detailed True}

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