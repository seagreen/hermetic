-- | Internal.
module JsonRelay.Shared where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word
import Network.Socket (AddrInfo(..), SockAddr(..), getAddrInfo)
import Prelude
import Streaming
import qualified Streaming.Prelude as S

-- | An invalid UTF-8 byte, used to signal the end of a 'Message'.
--
-- Since multiple 'Message's can be read on each call to 'NetworkBts.recvFrom',
-- each 'Message' must be followed by an @endOfMessage@ to distinguish them.
endOfMessage :: Word8
endOfMessage =
  0xFF

-- | Comment from @Network.Socket.ByteString@:
--
-- @
-- Considering hardware and network realities, the maximum number of bytes to receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has closed its half side of the connection.
-- @
maxBytes :: Int
maxBytes =
  4096

data Message = Message
  { messageRoom :: RoomName
  , messageBody :: Value
  } deriving (Eq, Show)

newtype RoomName
  = RoomName { unRoomName :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

instance ToJSON Message where
  toJSON m =
    object
      [ "room" .= messageRoom m
      , "body" .= messageBody m
      ]

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \o ->
      Message <$> o .: "room" <*> o .: "body"

data MessagePart
  = Complete ByteString SockAddr
  | Partial ByteString SockAddr

splitMessages :: (ByteString, SockAddr) -> Stream (Of MessagePart) IO ()
splitMessages (bts, addr) = do
  case reverse (BS.split endOfMessage bts) of
    [] -> pure ()
    x:xs -> S.each (reverse (Partial x addr : fmap (\y -> Complete y addr) xs))

getOneAddrInfo :: Maybe AddrInfo -> Maybe String -> Maybe String -> IO AddrInfo
getOneAddrInfo hints hostName serviceName = do
  addrs <- getAddrInfo hints hostName serviceName
  case addrs of
    [] ->
      error "getAddrInfo didn't throw an exception on empty list"

    addr:_ -> do
      pure addr

-- | Copied from `network` because it wasn't exported.
--
-- Shows the fields of 'defaultHints', without inspecting the by-default undefined fields 'addrAddress' and 'addrCanonName'.
showDefaultHints :: AddrInfo -> String
showDefaultHints a =
  concat
    [ "AddrInfo {"
    , "addrFlags = "
    , show (addrFlags a)
    , ", addrFamily = "
    , show (addrFamily a)
    , ", addrSocketType = "
    , show (addrSocketType a)
    , ", addrProtocol = "
    , show (addrProtocol a)
    , ", addrAddress = "
    , "<assumed to be undefined>"
    , ", addrCanonName = "
    , "<assumed to be undefined>"
    , "}"
    ]
