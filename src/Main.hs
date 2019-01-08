{-# LANGUAGE TemplateHaskell #-}

-- | Parse command line options into a runnable program.
--
-- We do this in the @library@ part of the package so readers of the
-- Haddocks can see the whole program with nothing hidden.
--
-- However, we can't build exectuables from @library@ functions, so there's
-- also an @executable@ with one module @./misc/Main.hs@, which re-exposes
-- @main@ from here.
module Main
  ( main
  , configParser
  ) where

import App (Config(..), app)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Game.Prelude
import JsonRelay.Client (RoomName(..))
import Model (Scenario(..))
import Options.Applicative
import Paths_hermetic (version)

main :: IO ()
main =
  app =<< args

args :: IO Config
args =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo Config
configParser =
  info
    (helper <*> versionOption <*> parser)
    fullDesc
  where
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption
        (showVersion version <> " " <> $(gitHash))
        (  long "version"
        <> help "Show version"
        )

    parser :: Parser Config
    parser =
      Config
        <$> option str
            (  long "host"
            <> metavar "HOST"
            <> help "Server address to connect to"
            <> value "localhost"
            -- showDefaultWith instead of showDefault so we don't get quotes
            -- around the value:
            <> showDefaultWith T.unpack
            )
        <*> option auto
            (  long "port"
            <> metavar "PORT"
            <> help "Server port to connect to"
            <> value 3000
            <> showDefault
            )
        <*> option (maybeReader (Just . RoomName . T.pack))
            (  long "room"
            <> metavar "ROOM"
            <> help "Room name to join"
            <> value (RoomName "room1")
            -- showDefaultWith instead of showDefault so we don't get:
            --
            --     default: RoomName {unRoomName = "room1"}
            --
            <> showDefaultWith (T.unpack . unRoomName)
            )
        <*> option (maybeReader scenarioParser)
            (  long "map"
            <> metavar "MAP"
            <> help "Scenario to play"
            <> value Tannen
            <> showDefaultWith scenarioSerializer
            )
        <*> switch
            (  long "sandbox"
            <> help "Play locally against a computer that doesn't move"
            )

scenarioSerializer :: Scenario -> [Char]
scenarioSerializer = \case
  Tannen -> "tannen"
  Polar -> "polar"

scenarioParser :: [Char] -> Maybe Scenario
scenarioParser t =
  Map.lookup t stringToScenario
  where
    stringToScenario :: Map [Char] Scenario
    stringToScenario =
      let f scenario = (scenarioSerializer scenario, scenario)
      in Map.fromList (f <$> [minBound .. maxBound])
