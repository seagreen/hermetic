{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import JsonRelay.Server (Config(..), run)
import Options.Applicative
import Paths_json_relay (version)
import Prelude

main :: IO ()
main =
  run =<< runParser

runParser :: IO Config
runParser =
  customExecParser (prefs showHelpOnError) parserInfo

parserInfo :: ParserInfo Config
parserInfo =
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
        <$> option auto
            (  long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Self-explanatory"
            <> value 3000
            <> showDefault
            )
