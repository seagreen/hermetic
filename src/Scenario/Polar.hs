module Scenario.Polar where

import Control.Monad.Trans.State
import qualified Data.Set as Set
import Game
import Game.Prelude
import Scenario.Tannen (defPlace, defShip, newPlace)

fillBoard :: State Model ()
fillBoard = do
  p1 <- newPlace $ def (-388,-388) "Ajos" & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player1
                                          & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                          & placeTypeL . _PBase . baseShieldsL .~ startingShields

  _ <- newPlace $ def (-200,-200) "Nyby"
  _ <- newPlace $ def (0,0) "Tervola" & placeSizeL .~ Small
  _ <- newPlace $ def (200,200) "Hosio"
  p2 <- newPlace $ def (388,388) "Sangis" & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player2
                                          & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                          & placeTypeL . _PBase . baseShieldsL .~ startingShields

  _ <- newPlace $ def (0,-400) "Muurola"
  _ <- newPlace $ def (-400,0) "Pursu"
  _ <- newPlace $ def (400,0) "Ekfors"
  _ <- newPlace $ def (0,400) "Vaski"
  _ <- newPlace $ def (-282,282) "Greus" & placeSizeL .~ Large
  _ <- newPlace $ def (282,-282) "Narkaus" & placeSizeL .~ Large

  _ <- newShip $ defShip Player1 (AtBase p1)
  _ <- newShip $ defShip Player2 (AtBase p2)

  pure ()
  where
    def = defPlace
