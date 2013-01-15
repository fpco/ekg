{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module System.Remote.Wai
    ( startServer
    , serveCombined
    , serveMany
    , serveOne
    , serveAll
    , serveFile
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.HashMap.Strict as M
import           Data.IORef (IORef)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Filesystem.Path.CurrentOS hiding (null)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Paths_ekg (getDataDir)
import           Prelude hiding (read)
import           System.Remote.Common

------------------------------------------------------------------------

startServer :: IORef Counters -> IORef Gauges -> IORef Labels
            -> B.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer counters gauges labels host port =
    runSettings conf (monitor counters gauges labels)
  where conf = defaultSettings
                   { settingsHost = Host (BC.unpack host)
                   , settingsPort = port
                   }

jsonHeaders :: ResponseHeaders
jsonHeaders = [("Content-Type", "application/json")]

textHeaders :: ResponseHeaders
textHeaders = [("Content-Type", "text/plain")]

monitor :: IORef Counters -> IORef Gauges -> IORef Labels -> Application
monitor counters gauges labels req
    | wantJson && matchPath "combined" =
        serveCombined counters gauges labels req
    | wantJson && matchPath "counters" = serveMany counters req
    | wantText && matchPath "counters" = serveOne counters req
    | wantJson && matchPath "gauges" = serveMany gauges req
    | wantText && matchPath "gauges" = serveOne gauges req
    | wantJson && matchPath "labels" = serveMany labels req
    | wantText && matchPath "labels" = serveOne labels req
    | wantJson = serveAll counters gauges labels req
    | otherwise = serveFile req
  where
    wantJson = acceptingType "application/json" req
    wantText = acceptingType "text/plain" req
    matchPath str = not (null (pathInfo req)) && str == head (pathInfo req)

serveFile req = do
    dataDir <- fromText . T.pack <$> liftIO getDataDir
    staticApp (defaultFileServerSettings (dataDir </> "assets")) req

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe B.ByteString
acceptHeader req = lookup "Accept" (requestHeaders req)

acceptingType :: B.ByteString -> Request -> Bool
acceptingType fmt req =
    let acceptHdr = List.head . parseHttpAccept <$> acceptHeader req
    in maybe False (== fmt) acceptHdr

serveWrapper ::
    (IORef Counters -> IORef Gauges -> IORef Labels -> IO BL.ByteString)
    -> IORef Counters -> IORef Gauges -> IORef Labels -> Application
serveWrapper f counters gauges labels _ = do
    bs <- liftIO $ f counters gauges labels
    return $ responseLBS status200 jsonHeaders bs
{-# INLINABLE serveWrapper #-}

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
serveAll :: IORef Counters -> IORef Gauges -> IORef Labels -> Application
serveAll = serveWrapper buildAll
{-# INLINABLE serveAll #-}

-- | Serve all counters and gauges, built-in or not, as a flattened
-- JSON object.
serveCombined :: IORef Counters -> IORef Gauges -> IORef Labels -> Application
serveCombined = serveWrapper buildCombined
{-# INLINABLE serveCombined #-}

-- | Serve a collection of counters or gauges, as a JSON object.
serveMany :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r) -> Application
serveMany mapRef _ = do
    bs <- liftIO $ buildMany mapRef
    return $ responseLBS status200 jsonHeaders bs
{-# INLINABLE serveMany #-}

-- | Serve a single counter, as plain text.
serveOne :: (Ref r t, Show t) => IORef (M.HashMap T.Text r) -> Application
serveOne refs req =
    case fmap T.decodeUtf8 <$> lookup "name" (queryString req) of
        Just (Just name) -> do
            mbs <- liftIO $ buildOne refs name
            case mbs of
                Just bs -> return $ responseLBS status200 textHeaders
                                               (lazyFromStrictB bs)
                Nothing -> notFound
        _ -> notFound
  where lazyFromStrictB = flip BLI.chunk BLI.Empty
{-# INLINABLE serveOne #-}

notFound = return $ responseLBS status404 textHeaders "Not found"
{-# INLINABLE notFound #-}
