-- | Server that bounces JSON messages between clients.
--
-- Clients send 'Message's.
--
-- 'messageRoom' is read to find the room, then the 'messageBody' is sent
-- to each other client subscribed to that room. So clients send
-- @Message@s and receive 'Value's.
--
-- Clients are subscribed to the first room they send a message to.
module JsonRelay.Server where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe (bracket)
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import JsonRelay.Shared
  (Message(..), MessagePart(..), RoomName(..), endOfMessage, getOneAddrInfo,
  maxBytes, splitMessages)
import Network.Socket
  (AddrInfo(..), AddrInfoFlag(..), SockAddr(..), Socket(..), SocketOption(..),
  SocketType(..))
import Numeric.Natural
import Prelude hiding (log)
import System.IO.Error (ioError, userError)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as NetworkBts
import qualified Streaming.Prelude as S
import qualified System.IO

data Config = Config
  { configPort :: Natural
  } deriving Show

newtype BroadcastChan
  = BroadcastChan { unBroadcastChan :: TChan (Message, SockAddr) }

log :: Text -> IO ()
log =
  T.hPutStrLn System.IO.stderr

run :: Config -> IO ()
run (Config port) = do

  addr <- resolve port

  chan <- BroadcastChan <$> newBroadcastTChanIO

  bracket
    (open addr)
    Socket.close
    -- Watch out for @(forever (acceptClient chan))@, which loops forever.
    (forever . acceptClient chan :: Socket -> IO void)

resolve :: Natural -> IO AddrInfo
resolve port = do
  addr <- getOneAddrInfo (Just hints) Nothing (Just (show port))
  log (T.unlines
        [ "Filled out AddrInfo:"
        , T.pack (show addr)
        , ""
        ])
  pure addr
  where
    hints :: AddrInfo
    hints =
      Socket.defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }

-- | @socket@, @bind@ and @listen@.
open :: AddrInfo -> IO Socket
open addr = do
  sock <- Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Socket.setSocketOption sock ReuseAddr 1
  Socket.bind sock (addrAddress addr)
  -- From @network@:
  --
  -- @
  -- If the prefork technique is not used, set CloseOnExec for the security reasons.
  -- @
  Socket.setCloseOnExecIfNeeded (Socket.fdSocket sock)
  Socket.listen sock maxQueuedConnections
  pure sock
  where
    -- From `network`:
    --
    -- @
    -- The second argument specifies the maximum number of queued connections
    -- and should be at least 1; the maximum value is system-dependent (usually 5).
    -- @
    maxQueuedConnections :: Int
    maxQueuedConnections =
      10

-- | @accept@
acceptClient :: BroadcastChan -> Socket -> IO ()
acceptClient chan sock = do
  (conn :: Socket, peer :: SockAddr) <- Socket.accept sock
  log $ "Connection from " <> T.pack (show peer)
  void $ forkFinally (handleClient peer chan conn) (\_ -> Socket.close conn)

-- | @recvFrom@ and @sendAll@.
--
-- From @network@:
--
-- @
-- If multiple threads use one Socket concurrently, unexpected things would happen.
-- There is one exception for multiple threads vs a single Socket:
-- one thread reads data from a Socket only and the other thread writes data to the Socket only.
-- @
handleClient :: SockAddr -> BroadcastChan -> Socket -> IO ()
handleClient addr chan sock = do
  readingChannel <- atomically $ dupTChan (unBroadcastChan chan)
  room <- newTVarIO Nothing
  race_
    (fromClient room sock chan)
    (forever (toClient room sock addr readingChannel))

toClient :: TVar (Maybe RoomName) -> Socket -> SockAddr -> TChan (Message, SockAddr) -> IO ()
toClient room sock sockAdr chan = do
  ((msg, addr), roomName) <-
    atomically $ (,) <$> readTChan chan <*> readTVar room

  when (addr /= sockAdr && Just (messageRoom msg) == roomName) $
    NetworkBts.sendAll sock $
      LBS.toStrict (  encode (messageBody msg)
                   <> LBS.singleton endOfMessage
                   )

fromClient :: TVar (Maybe RoomName) -> Socket -> BroadcastChan -> IO ()
fromClient room sock chan = do
  S.foldM_ next (pure []) (\_ -> pure ()) $
    S.for (S.untilRight receive) splitMessages
  where
    next :: [ByteString] -> MessagePart -> IO [ByteString]
    next remainder part = do
      case part of
        Complete bts addr ->
          let final = BS.concat (reverse (bts : remainder))
          in case eitherDecodeStrict final of
            Left e -> do
              ioError $ userError ("Error decoding message: " <> e)

            Right msg -> do
              process msg addr
              pure []

        Partial bts _ ->
          pure (bts : remainder)

    receive :: IO (Either (ByteString, SockAddr) ())
    receive = do
      (bts, addr) <- NetworkBts.recvFrom sock maxBytes
      if BS.null bts
        then
          pure $ Right ()

        else
          pure $ Left (bts, addr)

    process :: Message -> SockAddr -> IO ()
    process msg addr = do
      let name :: RoomName
          name = messageRoom msg
      oldName <-
        atomically $ do
          writeTChan (unBroadcastChan chan) (msg, addr)
          swapTVar room (Just name)
      when (oldName /= Just name) $
        log ("Address " <> T.pack (show addr) <> " subscribed to room: " <> unRoomName name)
