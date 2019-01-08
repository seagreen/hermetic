-- | Functions that are used by more than one @Game.Update.*@ module
-- or are likely to be generally useful go here.
module Game.Update.Shared where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude

startingShields :: Nat
startingShields =
  5

-- | We could replace @State Model@ with a newtype throughout the code
-- and make that newtype an instance of 'MonadRandom'. However in the
-- interests of sight-reading we avoid that and use this function
-- when we need randomness.
runRandom :: Rand StdGen a -> State Model a
runRandom go = do
  Gen stdGen <- use modelRandomL
  let (res, nextStdGen) = runRand go stdGen
  modelRandomL .= Gen nextStdGen
  pure res

-- | The Maybe is whether there are controlling ships or not.
--
-- There are controlling ships when only a single player has ships over the base.
--
-- An alternate way of implementing this would be to write:
--
-- @
--   basesWithControlStatus :: State Model [(PlaceId, Base, Maybe (Player, HashMap ShipId Ship))]
-- @
--
-- but when we consumed this using:
--
-- @
--   xs <- basesWithControlStatus
--   traverse_ foo xs
-- @
--
-- ... each @foo@ call would get statically frozen @Base@ and @HashMap ShipId Ship@
-- arguments, instead of ones that have been affected by earlier calls of @foo@.
-- This is correct for some circumstances, but we don't default to it because
-- the risk of overwriting new values with old ones is scary.
forBasesWithControlStatus
  :: (PlaceId -> Base -> Maybe (Player, HashMap ShipId Ship) -> State Model ())
  -> State Model ()
forBasesWithControlStatus action =
  traverse_ f =<< map HM.keys (use modelPlacesL)
  where
    f :: PlaceId -> State Model ()
    f placeId = do
      place <- getPlace placeId <$> use modelPlacesL
      case placeType place of
        Ruin ->
          pure ()

        PBase base -> do
          ships <- shipsAtPlace placeId <$> use modelShipsL
          let players :: Set Player
              players =
                Set.fromList $ shipPlayer <$> HM.elems ships

          case Set.toList players of
            [controllingPlayer] ->
              action placeId base (Just (controllingPlayer, ships))

            _ ->
              action placeId base Nothing

forControlledBases
  :: (PlaceId -> Base -> Player -> HashMap ShipId Ship -> State Model ())
  -> State Model ()
forControlledBases action =
  forBasesWithControlStatus f
  where
    f :: PlaceId -> Base -> Maybe (Player, HashMap ShipId Ship) -> State Model ()
    f placeId base mController = do
      case mController of
        Nothing ->
          pure ()

        Just (controllingPlayer, ships) ->
          action placeId base controllingPlayer ships

forOccupiedBases
  :: (PlaceId -> Base -> Player -> HashMap ShipId Ship -> State Model ())
  -> State Model ()
forOccupiedBases action =
  forBasesWithControlStatus f
  where
    f :: PlaceId -> Base -> Maybe (Player, HashMap ShipId Ship) -> State Model ()
    f placeId base mController = do
      case mController of
        Nothing ->
          pure ()

        Just (controllingPlayer, ships) ->
          if PlayerOwner controllingPlayer == baseOwner base
            then
              pure ()

            else
              action placeId base controllingPlayer ships

forUnoccupiedBases
  :: (PlaceId -> Base -> State Model ())
  -> State Model ()
forUnoccupiedBases action =
  forBasesWithControlStatus f
  where
    f :: PlaceId -> Base -> Maybe (Player, HashMap ShipId Ship) -> State Model ()
    f placeId base mController = do
      case mController of
        Nothing ->
          action placeId base

        Just (controllingPlayer, _) ->
          if PlayerOwner controllingPlayer == baseOwner base
            then
              action placeId base

            else
              pure ()

-- | A common enough operation to get its own function.
adjustBase :: PlaceId -> (Base -> Base) -> State Model ()
adjustBase id f =
  modelPlacesL %= HM.adjust (placeTypeL . _PBase %~ f) id

newId :: State Model Nat
newId = do
  n <- use modelNextIdL
  modelNextIdL += 1
  pure n

newShip :: Ship -> State Model ShipId
newShip ship = do
  id <- ShipId <$> newId
  modelShipsL %= HM.insert id ship
  pure id

canExpand :: Place -> Bool
canExpand place =
  case placeType place of
    Ruin ->
      False

    PBase base ->
      case placeSize place of
        Small ->
          basePopulation base < City

        Medium ->
          basePopulation base < Megacity

        Large ->
          basePopulation base < Ecumenopolis

minPop :: Population
minPop =
  Outpost

prevPop :: Population -> Population
prevPop = \case
  Outpost      -> Outpost
  Settlement   -> Outpost
  City         -> Settlement
  Megacity     -> City
  Ecumenopolis -> Megacity

incrementPop :: Place -> Place
incrementPop place =
  if canExpand place
    then place & placeTypeL . _PBase . basePopulationL %~ next
    else place
  where
    next :: Population -> Population
    next = \case
      Outpost      -> Settlement
      Settlement   -> City
      City         -> Megacity
      Megacity     -> Ecumenopolis
      Ecumenopolis -> Ecumenopolis

getShip :: ShipId -> HashMap ShipId Ship -> Ship
getShip id ships =
  case HM.lookup id ships of
    Just a -> a
    Nothing -> error "getShip failed"

getPlace :: PlaceId -> HashMap PlaceId Place -> Place
getPlace id bases =
  case HM.lookup id bases of
    Just a -> a
    _ -> error "getPlace failed"

shipsInFlight :: HashMap ShipId Ship -> HashMap ShipId (Ship, Point, PlaceId)
shipsInFlight =
  HM.mapMaybe f
  where
    f :: Ship -> Maybe (Ship, Point, PlaceId)
    f ship =
      case shipLocation ship of
        InFlight point dest _ -> Just (ship, point, dest)
        Destroyed -> Nothing
        AtBase _ -> Nothing

shipsAtPlace :: PlaceId -> HashMap ShipId Ship -> HashMap ShipId Ship
shipsAtPlace placeId =
  HM.mapMaybe f
  where
    f :: Ship -> Maybe Ship
    f ship = do
      loc <- case shipLocation ship of
               InFlight{} -> Nothing
               Destroyed -> Nothing
               AtBase a -> Just a
      guard (loc == placeId)
      pure ship
