{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Logger.GoogleLogging where

import Data.IP (fromHostAddress, fromIPv4, fromIPv6, fromHostAddress6)
import Network.Socket (SockAddr(..), HostAddress6, HostAddress)
import GHC.Int (Int64)
import Network.Google.Logging (LogEntry)
import Network.HTTP.Types.Status (Status(..))
import Control.Lens ((&), (.~))
import Data.Text (Text)
import Data.Time
       (UTCTime, getCurrentTime, formatTime, defaultTimeLocale,
        diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad.Log (Handler)
import Network.Wai (Middleware, Request, Response, RequestBodyLength(..))

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.Wai as Wai
import qualified Network.Google.Logging as Logging


loggerMiddleware
    :: Handler IO LogEntry -> Middleware
loggerMiddleware logger app req respond = do
    requestTime <- getCurrentTime
    app req $
        \res -> do
            responseTime <- getCurrentTime
            _ <-
                logger
                    (fillLogEntry
                         (responseTime `diffUTCTime` requestTime)
                         responseTime
                         req
                         res)
            respond res

-- | Helper function for fill LogEntry fields with data from
-- Request and Response
fillLogEntry :: NominalDiffTime -> UTCTime -> Request -> Response -> LogEntry
fillLogEntry lat currentTime req res =
    (((Logging.logEntry & Logging.leHTTPRequest .~ (pure fillHTTPRequest))
                        & Logging.leSeverity .~ (pure (statusSeverity (Wai.responseStatus res))))
                        & Logging.leTimestamp .~ (pure currentTime))
  where
    fillHTTPRequest =
        (((((((((Logging.hTTPRequest) & Logging.httprLatency .~ (pure (fromRational (toRational lat))))
                                      & Logging.httprReferer .~ (Text.decodeUtf8 <$> Wai.requestHeaderReferer req))
                                      & Logging.httprRemoteIP .~ (pure (ipFromSockAddr (Wai.remoteHost req))))
                                      & Logging.httprUserAgent .~ (Text.decodeUtf8 <$> Wai.requestHeaderUserAgent req))
                                      & Logging.httprStatus .~ (pure (fromIntegral (statusCode (Wai.responseStatus res)))))
                                      & Logging.httprRequestSize .~ (pure (rblToInt (Wai.requestBodyLength req))))
                                      & Logging.httprRequestURL .~ (pure (Text.decodeUtf8 (Wai.rawPathInfo req))))
                                      & Logging.httprRequestMethod .~ (pure (Text.decodeUtf8 (Wai.requestMethod req))))


-- | Helper function for extract client IP address
ipFromSockAddr :: SockAddr -> Text
ipFromSockAddr (SockAddrInet _ ha) = showIPv4 ha
ipFromSockAddr (SockAddrInet6 _ _ ha _) = showIPv6 ha
ipFromSockAddr (SockAddrUnix sock) = Text.pack sock
ipFromSockAddr (SockAddrCan i) = Text.pack (show i)

-- | Define log severity depend on response status.
-- when 1xx then INFO
--      2xx - DEFAULT
--      3xx - NOTICE
--      4xx - ERROR
--      5xx - CRITICAL
statusSeverity :: Status -> Logging.LogEntrySeverity
statusSeverity (Status code _) =
    if code < 200
        then Logging.Info
        else if (code < 300)
                 then Logging.Default
                 else if (code < 400)
                          then Logging.Notice
                          else if (code < 500)
                                   then Logging.Error'
                                   else Logging.Critical

-- | Conver Wai RequestBodyLength to Integer
-- when rbl is chuncked body we set size to 0 another return known length
rblToInt :: RequestBodyLength -> Int64
rblToInt ChunkedBody = 0
rblToInt (KnownLength kl) = fromIntegral kl

-- | Grab from <https://hackage.haskell.org/package/wai-extra-3.0.17/docs/src/Network-Wai-Middleware-RequestLogger-JSON.html#formatAsJSON here>
-- because don't want add wai-extra dependencies for one function
showIPv4 :: HostAddress -> Text
showIPv4 =
    Text.intercalate "." . map (Text.pack . show) . fromIPv4 . fromHostAddress

showIPv6 :: HostAddress6 -> Text
showIPv6 =
    Text.intercalate ":" . map (Text.pack . show) . fromIPv6 . fromHostAddress6

-- | A duration in seconds with up to nine fractional digits,
-- terminated by 's'. Example: "3.5s".
showDuration :: NominalDiffTime -> Text
showDuration =
    Text.pack . formatTime defaultTimeLocale "%s%Qs" . posixSecondsToUTCTime
