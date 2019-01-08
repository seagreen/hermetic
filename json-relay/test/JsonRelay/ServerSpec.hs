{-# LANGUAGE NumericUnderscores #-}

module JsonRelay.ServerSpec where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Aeson
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.IO as T
import JsonRelay.Client (Client(..))
import qualified JsonRelay.Client as Client
import qualified JsonRelay.Server as Server
import qualified JsonRelay.Shared as Shared
import Numeric.Natural
import Prelude hiding (log)
import qualified System.IO
import Test.Hspec

testPort :: Natural
testPort =
  41979 -- arbitrary

spec :: Spec
spec = do
  describe "The client and server" $ do
    it "perform basic tasks correctly" $ do
      race_
        (Server.run (Server.Config testPort))
        (threadDelay 500_000 *> runClients)
  where
    runClients :: IO ()
    runClients =
      concurrently_
        (testClient f)
        (threadDelay 500_000 *> testClient g)

testClient :: (Client -> IO ()) -> IO ()
testClient =
  Client.run log "localhost" testPort (Shared.RoomName "test")

f :: Client -> IO ()
f Client{clientSend, clientReceive} = do
  clientSend (String "hello")

  -- Test small messages. recvFrom can return multiple of these
  -- in one call.

  receive "foo"
  receive "bar"
  receive "baz"

  -- Test large messages, which are split up over multiple
  -- recvFrom calls.

  receive largeStr1
  receive largeStr2
  receive largeStr3

  receive "quux"
  where
    receive :: Text -> IO ()
    receive expected = do
      res <- clientReceive
      case eitherDecodeStrict res of
        Left e ->
          error ("Couldn't decode payload: " <> e)

        Right t ->
          t `shouldBe` expected

g :: Client -> IO ()
g Client{clientSend} = do
  clientSend (String "foo")
  clientSend (String "bar")
  clientSend (String "baz")

  clientSend (String largeStr1)
  clientSend (String largeStr2)
  clientSend (String largeStr3)

  clientSend (String "quux")

  -- An unexpected extra message that should be ignored.
  clientSend (String largeStr1)

largeStr1 :: Text
largeStr1 =
  fold ("a" : "b" : replicate 20_000 "c")

largeStr2 :: Text
largeStr2 =
  largeStr1 <> "d"

largeStr3 :: Text
largeStr3 =
  largeStr2 <> "e"

log :: Text -> IO ()
log =
  T.hPutStrLn System.IO.stderr
