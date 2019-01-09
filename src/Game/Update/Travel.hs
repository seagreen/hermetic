module Game.Update.Travel
  ( shipSpeed
  , shipsEmbark
  , shipsTravel
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | Determined by ship type and whether the base it last
-- left had a 'Booster' (if friendly).
shipSpeed :: Ship -> IsBoosted -> Float
shipSpeed ship isBoosted =
  case isBoosted of
    NotBoosted -> baseSpeed
    Boosted -> baseSpeed * 2
  where
    baseSpeed :: Float
    baseSpeed =
      case shipType ship of
        Corvette ->
          100

        Station ->
          100 / 3

        Monitor ->
          100

-- | __Player guide previous__: 'Game.Update.Bombard.bombard'
--
-- You can only move ships that are at a base. Once a ship has started moving
-- it can't change course, instead it proceeds to its destination.
--
-- You can't see enemy ships in flight (or at bases that aren't either
-- friendly or have a friendly ship present).
--
-- __Next__: 'Game.Update.Disease.diseaseSpread'
shipsEmbark :: HashMap Player (HashMap ShipId PlaceId) -> State Model ()
shipsEmbark =
  hmTraverseWithKey_ playerShipsEmbark

-- | Ships that have been given travel orders switch 'shipLocation'
-- from 'AtBase' to 'InFlight'.
--
-- We avoid the word \"move\" because it can mean different things.
-- Instead, ships \"embark\" and \"travel\" and players give \"orders\".
playerShipsEmbark :: Player -> HashMap ShipId PlaceId -> State Model ()
playerShipsEmbark _ = do
  traverse_ f . hmToList
  where
    f :: (ShipId, PlaceId) -> State Model ()
    f (shipId, destId) = do
      ship <- getShip shipId <$> use modelShipsL
      case shipLocation ship of
        InFlight{} ->
          pure ()

        Destroyed ->
          pure ()

        AtBase placeId -> do
          place <- getPlace placeId <$> use modelPlacesL
          let isBoosted =
                case placeType place of
                  Ruin ->
                    NotBoosted

                  PBase base ->
                    let friendly = PlayerOwner (shipPlayer ship) == baseOwner base
                        booster = Set.member Booster (baseInstallations base)
                    in if friendly && booster
                      then Boosted
                      else NotBoosted

          modelShipsL %=
            HM.adjust
              (shipLocationL .~ InFlight (placePoint place) destId isBoosted)
              shipId

-- | Ships in flight move across the board, perhaps arriving
-- at their destinations.
shipsTravel :: State Model ()
shipsTravel = do
  hmTraverseWithKey_ travel =<< use modelShipsL
  where
    travel :: ShipId -> Ship -> State Model ()
    travel shipId ship = do
      case shipLocation ship of
        AtBase _ -> pure ()
        Destroyed -> pure ()
        InFlight loc destId isBoosted -> do
          dest <- getPlace destId <$> use modelPlacesL
          let
            speed :: Float
            speed =
              shipSpeed ship isBoosted

            destPoint :: Point
            destPoint =
              placePoint dest

            newLoc :: ShipLocation
            newLoc =
              if distance loc destPoint <= speed
                then
                  AtBase destId
                else
                  InFlight (travelTowards loc destPoint speed) destId isBoosted

          modelShipsL %= HM.insert shipId (ship & shipLocationL .~ newLoc)

travelTowards :: Point -> Point -> Float -> Point
travelTowards currentLocation@(x,y) destPoint speed =
  ( x + deltaX
  , y + deltaY
  )
  where
    deltaX :: Float
    deltaY :: Float
    (deltaX, deltaY) =
      deltas angle speed

    angle :: Float
    angle =
      angleBetweenPoints currentLocation destPoint
