module Game.Update.Combat
  ( combat
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | __Player guide previous__: 'Game.Update.Diplomacy.diplomacy'
--
-- When both players have ships at the same base they fight.
--
-- First the number of hits landed this turn is calculated for both sides.
-- Then they're distributed over the opponent's ships.
--
-- The first hit on a ship destroys its shields, the second destroys
-- the ship itself.
--
-- __Next__: 'Game.Update.Bombard.bombard'
combat :: State Model ()
combat = do
  bases <- use modelPlacesL
  ships <- use modelShipsL
  traverse_ (checkForCombat ships) (HM.keys bases)
  where
    checkForCombat :: HashMap ShipId Ship -> PlaceId -> State Model ()
    checkForCombat allShips placeId = do
      let ships :: HashMap ShipId Ship
          ships =
            shipsAtPlace placeId allShips
      let players = Set.map shipPlayer (Set.fromList (HM.elems ships))
      case Set.toList players of
        [] ->
          pure ()

        [_] ->
          pure ()

        _ ->
          baseCombat placeId ships

baseCombat
  :: PlaceId
  -> HashMap ShipId Ship
     -- ^ Ships at this base
  -> State Model ()
baseCombat placeId ships = do
  modelLogL . logCombatL %= HM.insert placeId mempty
  let (p1, p2) = playerShips ships

  p1Hits <- numberOfHits p1
  p2Hits <- numberOfHits p2

  (p1Destroyed, p1Remaining) <- runRandom $ distributeHits resultOfHit p1 p2Hits
  (p2Destroyed, p2Remaining) <- runRandom $ distributeHits resultOfHit p2 p1Hits

  updateDamaged (p1Remaining <> p2Remaining)

  removeDestroyed (p1Destroyed <> p2Destroyed)
  where
    resultOfHit :: Ship -> Maybe Ship
    resultOfHit ship =
      if shipShields ship
        then Just (ship { shipShields = False })
        else Nothing

    updateDamaged :: HashMap ShipId Ship -> State Model ()
    updateDamaged damaged =
      modelShipsL %= HM.union damaged

    removeDestroyed :: HashMap ShipId Ship -> State Model ()
    removeDestroyed xs =
      for_ (HM.keys xs) $ \id -> do
        modelShipsL %= HM.adjust (shipLocationL .~ Destroyed) id
        modelLogL . logCombatL %= HM.adjust (Set.insert id) placeId

playerShips :: HashMap ShipId Ship -> (HashMap ShipId Ship, HashMap ShipId Ship)
playerShips ships =
  let p1 = filter (\(_,s) -> shipPlayer s == Player1) $ HM.toList ships
      p2 = filter (\(_,s) -> shipPlayer s == Player2) $ HM.toList ships
  in (HM.fromList p1, HM.fromList p2)

numberOfHits :: forall id. HashMap id Ship -> State Model Natural
numberOfHits firingShips =
  sum <$> traverse f (stationBonus <> HM.elems firingShips)
  where
    f :: Ship -> State Model Natural
    f ship = do
      case shipType ship of
        Corvette -> fire
        Station -> fire
        Monitor -> do
          a <- fire
          b <- fire
          c <- fire
          pure (a + b + c)

    fire :: State Model Natural
    fire = do
      isHit <- runRandom (probability 0.5)
      pure $ if isHit
               then 1
               else 0

    -- A player's first station in a battle fires two extra times,
    -- their second one extra time.
    stationBonus :: [Ship]
    stationBonus =
      let stations = HM.filter (\ship -> shipType ship == Station) firingShips
      in case HM.elems stations of
        station1:station2:_ -> [station1, station1, station2]
        station:[]          -> [station, station]
        []                  -> []
