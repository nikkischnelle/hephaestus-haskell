module Middleware (limitRequestSize, logStdout, fileLogger, stdOutLogger) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger as Logger
import qualified Network.HTTP.Types as Http
import Data.ByteString.Builder (stringUtf8)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(AppendMode))
import Control.Monad (when)
import System.Info (os)

fileLogger :: FilePath -> IO Middleware
fileLogger path = do
    let actualPath = if path == "" then nullDevice else path

    handle <- openFile actualPath AppendMode
    let loggerSettings = defaultRequestLoggerSettings { outputFormat = Apache FromSocket , destination = Logger.Handle handle }
    mkRequestLogger loggerSettings
    where
        nullDevice = if os == "mingw32" then "\\\\.\\NUL" else "/dev/null"

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