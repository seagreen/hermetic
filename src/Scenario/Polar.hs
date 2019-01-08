module Scenario.Polar where

import Control.Monad.Trans.State
import qualified Data.Set as Set
import Game
import Game.Prelude
import Scenario.Tannen (defPlace, newPlace)

fillBoard :: State Model ()
fillBoard = do
  _ <- newPlace $ def (-388,-388) "A" & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player1
                                      & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                      & placeTypeL . _PBase . baseShieldsL .~ startingShields

  _ <- newPlace $ def (-200,-200) "B"
  _ <- newPlace $ def (0,0) "C" & placeSizeL .~ Small
  _ <- newPlace $ def (200,200) "D"
  _ <- newPlace $ def (388,388) "E" & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player2
                                    & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                    & placeTypeL . _PBase . baseShieldsL .~ startingShields

  _ <- newPlace $ def (0,-400) "F"
  _ <- newPlace $ def (-400,0) "G"
  _ <- newPlace $ def (400,0) "H"
  _ <- newPlace $ def (0,400) "I"
  _ <- newPlace $ def (-282,282) "J" & placeSizeL .~ Large
  _ <- newPlace $ def (282,-282) "K" & placeSizeL .~ Large

  pure ()
  where
    def = defPlace
