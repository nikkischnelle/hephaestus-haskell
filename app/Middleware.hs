module Middleware (limitRequestSize, logStdout, fileLogger, stdOutLogger) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger as Logger
import qualified Network.HTTP.Types as Http
import Data.ByteString.Builder (stringUtf8)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(AppendMode))
import Control.Monad (when)
import System.Info (os)

-- | simple logger that writes to file at specified path.
--   if path is empty, log to /dev/null on linux and NUL on windows
fileLogger :: FilePath -> IO Middleware
fileLogger path = do
    -- if the path is emtpy, replace with /dev/null on linux and NUL on windows
    let actualPath = if path == "" then nullDevice else path

    handle <- openFile actualPath AppendMode
    -- set up logger settings for specified path using the file handle
    let loggerSettings = defaultRequestLoggerSettings { outputFormat = Apache FromSocket , destination = Logger.Handle handle }
    mkRequestLogger loggerSettings
    
    where
        nullDevice = if os == "mingw32" then "\\\\.\\NUL" else "/dev/null"

-- | simple logger that prints to stdout
stdOutLogger :: IO Middleware
stdOutLogger = mkRequestLogger defaultRequestLoggerSettings {outputFormat = Detailed True}

-- | Limit the request size to 50mB
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