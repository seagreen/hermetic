-- | A library for connecting to the server.
--
-- Used by @hermetic@ but not the @json-relay@ executable.
module JsonRelay.Client
  ( RoomName(..)
  , Client(..)
  , run
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (bracket)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import JsonRelay.Shared
  (Message(..), MessagePart(..), RoomName(..), endOfMessage, getOneAddrInfo,
  maxBytes, splitMessages)
import Network.Socket (AddrInfo(..), SockAddr(..), Socket(..), SocketType(..))
import Numeric.Natural
import Prelude hiding (log)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as NetworkBts
import qualified Streaming.Prelude as S

data Client = Client
  { clientSend :: Value -> IO ()
  , clientReceive :: IO ByteString
    -- ^ The @ByteString@ will always be JSON.
  }

run
  :: (Text -> IO ())
     -- ^ Logger.
     --
     -- Present since module is meant to be used as a library
     -- (unlike "JsonRelay.Server").
  -> Text
     -- ^ Host.
  -> Natural
     -- ^ Port.
  -> RoomName
  -> (Client -> IO ())
  -> IO ()
run log host port room client = do
  addr <- resolve host (show port)
  log (T.pack (show addr))
  bracket (open addr) Socket.close (withSocket log room client)

resolve :: Text -> String -> IO AddrInfo
resolve hostName serviceName = do
  getOneAddrInfo (Just hints) (Just (T.unpack hostName)) (Just serviceName)
  where
    -- @showDefaultHints@ on this gives:
    --
    -- @
    -- AddrInfo {addrFlags = [], addrFamily = AF_UNSPEC, addrSocketType = Stream, addrProtocol = 0,
    --           addrAddress = <assumed to be undefined>, addrCanonName = <assumed to be undefined>}
    -- @
    hints :: AddrInfo
    hints =
      Socket.defaultHints { addrSocketType = Stream }

-- Internal.
--
-- @socket@ and @connect@.
open :: AddrInfo -> IO Socket
open addr = do
  sock <- Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Socket.connect sock (addrAddress addr)
  pure sock

-- Internal.
--
-- @recvFrom@ and @sendAll@.
withSocket :: (Text -> IO ()) -> RoomName -> (Client -> IO ()) -> Socket -> IO ()
withSocket log room withClient sock = do
  chan <- newTChanIO
  _ <- forkIO (fill chan)
  withClient (Client clientSend (clientReceive chan))
  where
    clientSend :: Value -> IO ()
    clientSend v = do
      log "Sending message"
      NetworkBts.sendAll
        sock
        (  LBS.toStrict (encode (Message room v))
        <> BS.singleton endOfMessage
        )

    clientReceive :: TChan ByteString -> IO ByteString
    clientReceive =
      atomically . readTChan

    fill :: TChan ByteString -> IO ()
    fill chan =
      S.foldM_ (next chan) (pure []) (\_ -> pure ()) $
        S.for (S.untilRight receive) splitMessages

    next :: TChan ByteString -> [ByteString] -> MessagePart -> IO [ByteString]
    next chan remainder part =
      case part of
        Complete bts _ -> do
          atomically $ writeTChan chan (BS.concat (reverse (bts : remainder)))
          pure []

        Partial bts _ -> do
          pure (bts : remainder)

    receive :: IO (Either (ByteString, SockAddr) ())
    receive = do
      (bts, addr :: SockAddr) <- NetworkBts.recvFrom sock maxBytes
      if BS.null bts
        then do
          log $ "Empty message from " <> T.pack (show addr)
          pure $ Right ()

        else do
          log $ "Message from " <> T.pack (show addr)
          pure $ Left (bts, addr)
