-- | Wrap 'Model.init', 'View.view', and 'Update.update' into a
-- function @app :: Config -> IO ()@.
--
-- In Elm this would be the @Main@ module, but Haskell reserves that name
-- for the module containing the executabilizable @main@ function. We're
-- not ready to that yet until we also have code to parse a 'Config'
-- from CLI arguments. That will come one more module downstream.
module App
  ( Config(..)
  , app
  , runGame
  , RoomMsg(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State.Lazy
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Game
import Game.Prelude
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game (Display(FullScreen), black)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import JsonRelay.Client (Client(..), RoomName(..))
import qualified JsonRelay.Client as Client
import Model
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import qualified System.IO
import Update
import View

data Config = Config
  { configHost :: Text
  , configPort :: Nat
  , configRoom :: RoomName
  , configScenario :: Scenario
  , configSandbox :: Bool
  } deriving Show

app :: Config -> IO ()
app (Config host port room scenario localSandbox) =
  if localSandbox
    then runSandbox scenario
    else runMultiplayer host port room scenario

runGame
  :: (Game.Player -> Game.Orders -> IO ())
  -> IO (Maybe (Game.Player, Game.Orders))
  -> Scenario
  -> Game.Player
  -> Game.Gen
  -> IO ()
runGame sendOrders pollOpponentOrders scenario player seed = do
  modelLogFile <- createModelLogFile
  screenSize <- startingScreenSize

  Gloss.playIO
    FullScreen
    black -- background color
    1 -- number of simulation steps to take for each second of real time
    (init seed screenSize scenario player)
    runView
    (runUpdate modelLogFile)
    runTickUpdate
  where
    runView :: Model -> IO Gloss.Picture
    runView =
      pure . view

    runUpdate :: FilePath -> Gloss.Event -> Model -> IO Model
    runUpdate modelLogFile event oldModel = do
      let m = update (UserEvent event) oldModel
      BS.writeFile modelLogFile (LBS.toStrict (encodePretty m))
      case updateResult oldModel m of
        Exit ->
          exitSuccess

        PlayerEndedTurn orders -> do
          sendOrders (modelWhoAmI m) orders
          pure m

        Normal ->
          pure m

    runTickUpdate :: Float -> Model -> IO Model
    runTickUpdate elapsed model = do
      mOrders <- pollOpponentOrders
      let newModel = case mOrders of
                       Nothing -> model
                       Just (p, o) -> update (OpponentOrders p o) model
      pure $ update (TimePassed elapsed) newModel

    startingScreenSize :: IO Box
    startingScreenSize = do
      (width, height) <- getScreenSize
      pure $ Box (realToFrac width) (realToFrac height)

runSandbox :: Scenario -> IO ()
runSandbox scenario = do
  seed <- Game.Gen <$> newStdGen
  runGame mockSend mockReceive scenario Game.Player1 seed
  where
    mockSend :: Game.Player -> Game.Orders -> IO ()
    mockSend _ _ =
      pure ()

    -- NOTE: This is sending a lot more opponent responses
    -- than usually expected (since it succeeds every time).
    --
    -- Sadly, it also sends them more slowly than desireable
    -- for single player. If you end turn more than once
    -- in a second you'll see the "waiting for opponent"
    -- notification.
    mockReceive :: IO (Maybe (Game.Player, Game.Orders))
    mockReceive =
      pure (Just (Game.Player2, mempty))

data RoomMsg
  = Announce
  | Existing Game.Gen Scenario
  | Turn Game.Player Game.Orders
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

runMultiplayer :: Text -> Nat -> RoomName -> Scenario -> IO ()
runMultiplayer host port room scenario =
  Client.run logTxt host port room (runWithClient scenario)

runWithClient :: Scenario -> Client -> IO ()
runWithClient scenario Client{clientSend, clientReceive} = do
  c <- newTChanIO
  void $ forkIO (forever (receive c))

  (player, chosenScenario, seed) <- start c

  runGame
    sendOrders
    (pollOpponentOrders c)
    chosenScenario
    player
    seed
  where
    sendOrders :: Game.Player -> Game.Orders -> IO ()
    sendOrders p o =
      clientSend (toJSON (Turn p o))

    receive :: TChan RoomMsg -> IO ()
    receive chan = do
      bts <- clientReceive
      case eitherDecodeStrict bts of
        Left e ->
          error ("couldn't decode bts" <> show e)

        Right msg ->
          atomically (writeTChan chan msg)

    start :: TChan RoomMsg -> IO (Game.Player, Scenario, Game.Gen)
    start chan = do
      clientSend (toJSON Announce)
      res <- atomically (readTChan chan)
      case res of
        Announce -> do
          seed <- Game.Gen <$> newStdGen
          clientSend (toJSON (Existing seed scenario))
          pure (Game.Player1, scenario, seed)

        Existing seed hostScenario ->
          pure (Game.Player2, hostScenario, seed)

        Turn{} ->
          error "unexpected turn message"

    pollOpponentOrders :: TChan RoomMsg -> IO (Maybe (Game.Player, Game.Orders))
    pollOpponentOrders chan = do
      res <- atomically (tryReadTChan chan)
      case res of
        Nothing ->
          pure Nothing

        Just (Turn player orders) ->
          pure $ Just (player, orders)

        Just _ -> error "wrong message type received"

createModelLogFile :: IO FilePath
createModelLogFile = do
  temp <- getTemporaryDirectory

  -- GUID appended so we don't conflict with anything:
  let ourDir = temp </> "hermetic-7c6d1406-ee0c-4c15-95c6-8cf401c20a8b"

  createDirectoryIfMissing False ourDir

  let ourFile = ourDir </> "model.json"

  logTxt ("Writing the model to:\n" <> T.pack ourFile)
  pure ourFile

logTxt :: Text -> IO ()
logTxt =
  T.hPutStrLn System.IO.stderr
