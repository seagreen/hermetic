module Game.Update.Bombard
  ( bombard
  , regenerateBaseShields
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | __Player guide previous__: 'Game.Update.Combat.combat'
--
-- When ships are present unopposed at an opponent's base, they reduce
-- its 'basePopulation' by one. If it can't go any lower (it's already
-- an 'Outpost') the base is destroyed.
--
-- __Next__: 'Game.Update.Travel.shipsEmbark'
bombard :: State Model ()
bombard =
  forOccupiedBases bombardPlace

bombardPlace :: PlaceId -> Base -> Player -> HashMap ShipId Ship -> State Model ()
bombardPlace placeId base _ ships =
  case baseOwner base of
    Neutral _ ->
      pure ()

    PlayerOwner _ ->
      if baseShields base > 0
        then damageShields
        else damageBase
  where
    damageShields :: State Model ()
    damageShields =
      if monitorIsPresent
        then do
          adjustBase placeId (baseInstallationsL %~ Set.delete Shield)
          adjustBase placeId (baseShieldsL .~ 0)

        else
          adjustBase placeId (baseShieldsL -~ 1)

    damageBase :: State Model ()
    damageBase =
      if monitorIsPresent || basePopulation base == minPop
        then
          modelPlacesL %= HM.adjust (placeTypeL .~ Ruin) placeId

        else
          adjustBase placeId (basePopulationL %~ prevPop)

    monitorIsPresent :: Bool
    monitorIsPresent =
      any (\ship -> shipType ship == Monitor) (HM.elems ships)

-- | Bases with a 'Shield' that aren't controlled by the opponent's
-- ships regenerate shields.
regenerateBaseShields :: State Model ()
regenerateBaseShields =
  forUnoccupiedBases regenerateShields

regenerateShields :: PlaceId -> Base -> State Model ()
regenerateShields placeId Base{..} =
  if Set.member Shield baseInstallations && baseShields < startingShields
    then adjustBase placeId (baseShieldsL +~ 1)
    else pure ()
