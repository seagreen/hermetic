module Game.Update.Build
  ( baseProduction
  , buildCost
  , buildOrSwitchBuilding
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | A base's production is determined by its 'Population'.
--
-- (An exception is 'Neutral' bases, which progress at a smaller, fixed rate
-- so they're not all at max population by the midgame).
baseProduction :: HashMap ShipId Ship -> Base -> Double
baseProduction shipsAtThisBase base =
  stationModifier $
    case basePopulation base of
      Outpost      -> 0.5
      Settlement   -> 0.75
      City         -> 1
      Megacity     -> 1.25
      Ecumenopolis -> 1.5
  where
    stationModifier :: Double -> Double
    stationModifier =
      case HM.keys (HM.filter friendlyStation shipsAtThisBase) of
        []  -> identity
        [_] -> (+) 0.1
        _   -> (+) 0.2

    friendlyStation :: Ship -> Bool
    friendlyStation ship =
      PlayerOwner (shipPlayer ship) == baseOwner base
       && shipType ship == Station

-- | Calculate a @BuildOrder@'s cost. 'BuildPopulation' costs more
-- the higher the next population, so requires the @Base@ argument
-- to calculate it.
buildCost :: Base -> BuildOrder -> Double
buildCost base = \case
  BuildShield -> 8
  BuildBooster -> 5
  BuildPopulation ->
    case basePopulation base of
      Outpost      -> 3
      Settlement   -> 5
      City         -> 7
      Megacity     -> 10
      Ecumenopolis -> 0
  BuildShip shipType ->
    case shipType of
      Corvette -> 5
      Station -> 8
      Monitor -> 15

-- | Bases that have been given a new 'BuildOrder' take a turn to switch.
-- Their progress towards old 'BuildOrder's is still saved.
--
-- Bases that haven't been given a new 'BuildOrder' progress towards what
-- they're currently building. If they complete it and can't build
-- any more of it (e.g. a base that just completed an 'Ecumenopolis')
-- they switch to 'Corvette's.
buildOrSwitchBuilding :: HashMap Player (HashMap PlaceId BuildOrder) -> State Model ()
buildOrSwitchBuilding orders = do
  places <- use modelPlacesL
  traverse_ (buildAtBase allBuildOrders) (HM.keys places)
  where
    allBuildOrders :: HashMap PlaceId BuildOrder
    allBuildOrders =
      fold (HM.elems orders)

buildAtBase :: HashMap PlaceId BuildOrder -> PlaceId -> State Model ()
buildAtBase buildOrders id = do
  switchOrProgress
  checkForCompletion id
  where
    switchOrProgress :: State Model ()
    switchOrProgress = do
      place <- getPlace id <$> use modelPlacesL
      case placeType place of
        Ruin ->
          pure ()

        PBase base ->
          case mBuildOrderChange base of
            Just newOrder ->
              switchWhatsBeingBuilt newOrder

            Nothing ->
              progress
      where
        mBuildOrderChange :: Base -> Maybe BuildOrder
        mBuildOrderChange base = do
          buildOrder <- HM.lookup id buildOrders
          guard (buildOrder /= baseBuilding base)
          pure buildOrder

        switchWhatsBeingBuilt :: BuildOrder -> State Model ()
        switchWhatsBeingBuilt newOrder =
          adjustBase id (baseBuildingL .~ newOrder)

        progress :: State Model ()
        progress = do
          ships <- shipsAtPlace id <$> use modelShipsL
          adjustBase id (addProduction ships)

addProduction :: HashMap ShipId Ship -> Base -> Base
addProduction shipsAtThisBase base@Base{..} =
  base & baseInProgressL %~ HM.insertWith (+) baseBuilding amount
  where
    amount :: Double
    amount =
      case baseOwner of
        Neutral _ ->
          0.1

        PlayerOwner _ ->
          baseProduction shipsAtThisBase base

checkForCompletion :: PlaceId -> State Model ()
checkForCompletion id = do
  place <- getPlace id <$> use modelPlacesL
  case placeType place of
    Ruin ->
      pure ()

    PBase base ->
      case HM.lookup (baseBuilding base) (baseInProgress base) of
        Nothing ->
          pure ()

        Just soFar -> do
          let wouldRemain :: Double
              wouldRemain =
                soFar - buildCost base (baseBuilding base)
          when (wouldRemain >= 0) $ complete base wouldRemain
  where
    complete :: Base -> Double -> State Model ()
    complete Base{..} remaining = do
      let f :: HashMap BuildOrder Double -> HashMap BuildOrder Double
          f = if remaining > 0
                then HM.insert baseBuilding remaining
                else HM.delete baseBuilding
      adjustBase id (baseInProgressL %~ f)
      case baseBuilding of
        BuildPopulation -> do
          modelPlacesL %= HM.adjust incrementPop id
          newPlace <- getPlace id <$> use modelPlacesL
          when (not (canExpand newPlace)) $
            -- NOTE: This gives a free switch to building ships,
            -- do we want that?
            --
            -- it also switches neutrals to building ships, so they'll seesaw
            -- because neutrals can't actually complete ships.
            adjustBase id (baseBuildingL .~ BuildShip Corvette)

        BuildShip shipType ->
          case baseOwner of
            Neutral _ ->
              adjustBase id (baseBuildingL .~ BuildPopulation)

            PlayerOwner player ->
              void $ newShip (Ship player (AtBase id) shipType True)

        BuildShield -> do
          adjustBase id ( (baseInstallationsL %~ Set.insert Shield)
                        . (baseShieldsL .~ startingShields)
                        )
          switchToCorvettes

        BuildBooster -> do
          adjustBase id (baseInstallationsL %~ Set.insert Booster)
          switchToCorvettes

    switchToCorvettes :: State Model ()
    switchToCorvettes =
      adjustBase id (baseBuildingL .~ BuildShip Corvette)
