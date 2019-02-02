module Game.Update
  ( update
  , updateM
  ) where

import Control.Monad.Trans.State
import Game.Model
import Game.Prelude
import qualified Game.Update.Bombard as Bombard
import qualified Game.Update.Build as Build
import qualified Game.Update.Combat as Combat
import qualified Game.Update.Diplomacy as Diplomacy
import qualified Game.Update.Disease as Disease
import qualified Game.Update.Travel as Travel

update :: HashMap Player Orders -> Model -> Model
update orders =
  execState (updateM orders)

updateM :: HashMap Player Orders -> State Model ()
updateM orders = do

  clearLog

  Travel.shipsEmbark (map ordersEmbark orders)

  Diplomacy.diplomacy
  Bombard.bombard
  Bombard.regenerateBaseShields

  Disease.diseaseSpread
  Disease.shipsHeal

  Travel.shipsTravel

  Combat.combat

  Build.build (map ordersBuild orders)

  modelTurnL += 1

clearLog :: State Model ()
clearLog =
  modelLogL .= mempty
