module Main where

import qualified "hermetic" Main as M
import Prelude

main :: IO ()
main =
  M.main
