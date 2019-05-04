module Game.Update.Build
  ( baseProduction
  , buildCost
  , build
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

-- | If a base completes what it's currently building and can't build
-- more of it (e.g. a base that just completed an 'Ecumenopolis')
-- then it switches to 'Corvette's.
build :: HashMap Player (HashMap PlaceId BuildOrder) -> State Model ()
build orders = do
  places <- use modelPlacesL
  for_ (HM.keys places) $ \id -> do
    switch placesToOrders id
    progress id
    checkForCompletion id
  where
    placesToOrders :: HashMap PlaceId (Player, BuildOrder)
    placesToOrders =
      let a1 :: [(Player, HashMap PlaceId BuildOrder)]
          a1 =
            HM.toList orders

          f :: ( Player
               , HashMap PlaceId BuildOrder
               )
            -> HashMap PlaceId (Player, BuildOrder)
          f (player, a) =
            map (\b -> (player, b)) a

          a2 :: [HashMap PlaceId (Player, BuildOrder)]
          a2 =
            map f a1

      in
        fold a2

switch :: HashMap PlaceId (Player, BuildOrder) -> PlaceId -> State Model ()
switch buildOrders id = do
  place <- getPlace id <$> use modelPlacesL
  case placeType place of
    Ruin ->
      -- NOTE: The UI should never send us building orders for ruins,
      -- so maybe a log statement should go here?
      pure ()

    PBase base ->
      case HM.lookup id buildOrders of
        Just (orderingPlayer, newOrder) ->
          -- NOTE: The UI should never send us building orders from a player
          -- who doesn't own a base, so maybe a log statement should go here?
          when (PlayerOwner orderingPlayer == baseOwner base) $
            adjustBase id (baseBuildingL .~ newOrder)

        Nothing ->
          pure ()

progress :: PlaceId -> State Model ()
progress id = do
  place <- getPlace id <$> use modelPlacesL
  case placeType place of
    Ruin ->
      pure ()

    PBase base -> do
      ships <- shipsAtPlace id <$> use modelShipsL
      let amount :: Double
          amount =
            case baseOwner base of
              Neutral _ ->
                0.1

              PlayerOwner _ ->
                baseProduction ships base
      adjustBase
        id
        (baseInProgressL %~ HM.insertWith (+) (baseBuilding base) amount)

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
            -- NOTE: This switches neutrals to building ships, so they'll seesaw
            -- because neutrals can't actually complete ships.
            adjustBase id (baseBuildingL .~ BuildShip Corvette)

        BuildShip shipType ->
          case baseOwner of
            Neutral _ ->
              adjustBase id (baseBuildingL .~ BuildPopulation)

            PlayerOwner player ->
              void $ newShip (Ship player (AtPlace id) shipType True)

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
