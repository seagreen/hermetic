module View.Board
  ( viewPlace
  , ruinPicture
  , viewBasePhoto
  , buildingText
  , viewShipInFlight
  , buildOrderText
  , ownerColor
  , playerName
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Game hiding (Model)
import Game.Prelude
import Layout
import Lib.Gloss
import Model
import View.Ship

viewPlace :: Model -> PlaceId -> Radius -> Picture
viewPlace m@Model{..} id radius =
  case placeType place of
    Ruin ->
      viewRuin m id radius

    PBase base ->
      viewBase m id base radius
  where
    place = getPlace id places

    places = modelPlaces modelGame

viewRuin :: Model -> PlaceId -> Radius -> Picture
viewRuin m id radius =
  fold
    [ ruinPicture
    , focusCircle m id radius
    , translateBelowBase radius $ verticalConcatText $
          viewText white "Ruin"
        : viewShipsAtBase m id
    ]

ruinPicture :: Picture
ruinPicture =
  foldMap craterLine [0, 45, 90, 135, 180, 225, 270, 315]
  where
    craterLine :: Float -> Picture
    craterLine degrees =
      Rotate degrees $ Color white $ Line [(0,20), (0, 30)]

viewBase :: Model -> PlaceId -> Base -> Radius -> Picture
viewBase m@Model{..} id base@Base{..} radius =
  fold
    [ viewBasePhoto base radius
    , focusCircle m id radius
    , translateBelowBase radius $
        verticalConcatText
          ( [ viewText (ownerColor baseOwner) (placeName place)
            , viewText white (T.pack (show basePopulation))
            , case baseOwner of
                Neutral attitude ->
                  -- Show attitude
                  case HM.lookup modelWhoAmI attitude of
                    Nothing ->
                      mempty

                    Just friendliness ->
                      viewText white ("Friendliness: " <> T.pack (show friendliness))

                PlayerOwner _ ->
                  -- Show production
                  viewText white ("Production: " <> atMostDecimals 2 (baseProduction shipsAtThisBase base))

            , if baseOwner == PlayerOwner modelWhoAmI
                then viewText white (buildingText modelOrders id base)
                else mempty
            , if baseShields /= 0 && baseShields < startingShields
                then viewText shieldColor $ "Shields: " <> T.pack (show baseShields)
                                                 <> "/" <> T.pack (show startingShields)
                else mempty
            ]
          <> viewShipsAtBase m id
          )
    , viewDisease modelTick baseDisease
    ]
  where
    place = getPlace id places

    places = modelPlaces modelGame

    shipsAtThisBase = shipsAtPlace id (modelShips modelGame)

viewBasePhoto :: Base -> Radius -> Picture
viewBasePhoto Base{..} radius =
  fold
    [ Color (ownerColor baseOwner) $ Circle (unRadius radius)
    , viewBaseShields radius baseShields
    ]

viewBaseShields :: Radius -> Nat -> Picture
viewBaseShields radius shields =
  if shields < 1
    then
      mempty
    else
      Color shieldColor $ Circle (unRadius radius - 4)

viewShipsAtBase :: Model -> PlaceId -> [Picture]
viewShipsAtBase Model{..} id =
  if hasDetection modelWhoAmI (Just place) ships
    then
      viewPlayerShips <$> Set.toList enumerateAll

    else
      mempty
  where
    viewPlayerShips :: Player -> Picture
    viewPlayerShips p =
      case snd <$> sortShipsForUI (HM.filter (\s -> shipPlayer s == p) ships) of
        [] -> mempty
        [x] -> Translate 15 5 $ viewShip x
        x:xs ->
          Translate 15 5 $
               viewShip x
            <> Translate 8 (-5)
                 (viewText
                    (playerColor p)
                    (" x" <> T.pack (show (length xs + 1))))

    ships :: HashMap ShipId Ship
    ships =
      shipsAtPlace id (modelShips modelGame)

    place :: Place
    place =
      getPlace id (modelPlaces modelGame)

focusCircle :: Model -> PlaceId -> Radius -> Picture
focusCircle m id radius =
  if isFocusedBase m id
    then Color yellow $ Circle (unRadius radius + 2)
    else mempty

translateBelowBase :: Radius -> Picture -> Picture
translateBelowBase (Radius radius) =
  Translate (negate (radius / 2)) (negate (radius + 20))

viewDisease :: Tick -> Disease -> Picture
viewDisease tick = \case
  Healthy ->
    mempty

  Latent ->
    fold
      [ Color orange $ ThickCircle 10 10
      , case tick of
          Tick -> mempty
          Tock ->
            Translate 40 0 $
              fold
                [ Translate 50 0 $
                    fold
                      [ Color black $ rectangleSolid 130 25
                      , Color orange $ rectangleWire 130 25
                      ]
                , verticallyCenteredText orange "Disease latent"
                ]
      ]

  Plague ->
    fold
      [ Color red $ ThickCircle 10 10
      , case tick of
          Tick -> mempty
          Tock ->
            Translate 40 0 $
              fold
                [ Translate 25 0 $
                    fold
                      [ Color black $ rectangleSolid 80 25
                      , Color orange $ rectangleWire 80 25
                      ]
                , verticallyCenteredText red "Plague"
                ]
      ]

buildingText :: Orders -> PlaceId -> Base -> Text
buildingText orders id base@Base{..} =
  case HM.lookup id (ordersBuild orders) of
    Nothing ->
      "Building: " <> T.toLower (buildOrderText baseBuilding) <> " " <> progress baseBuilding
    Just new ->
      "Switching to: " <> T.toLower (buildOrderText new) <> " " <> progress new
  where
    progress :: BuildOrder -> Text
    progress build =
      let n = case HM.lookup build baseInProgress of
                Nothing -> "0"
                Just a -> atMostDecimals 2 a
      in "(" <> n <> "/" <> atMostDecimals 2 (buildCost base build) <> ")"

viewShipInFlight :: Model -> ShipId -> Point -> Radius -> Picture
viewShipInFlight Model{..} id loc _
  | modelWhoAmI /= shipPlayer ship = mempty
  | otherwise =
      Rotate (negate (toDegrees angle)) $ fold
        [ viewShip ship
        , perhapsViewThrust
        ]
  where
    ship :: Ship
    ship = getShip id ships

    perhapsViewThrust :: Picture
    perhapsViewThrust =
      case isBoosted of
        Boosted ->
          thrustPicture

        NotBoosted ->
          case modelTick of
              Tick ->
                mempty

              Tock ->
                thrustPicture

    thrustPicture :: Picture
    thrustPicture =
        viewThrust (shipPlayer ship)
      & Scale (1/2) (1/2)

        -- move thrust behind the ship
        -- (the ship hasn't been rotated towards its destination yet).
      & Translate translateAmount 0

    translateAmount :: Float
    translateAmount =
      case shipType ship of
        Corvette ->
          -24

        Station ->
          -18

        Monitor ->
          -24

    angle :: Float
    angle =
      angleBetweenPoints loc destPoint

    destPoint :: Point
    isBoosted :: IsBoosted
    (destPoint, isBoosted) =
      case shipLocation ship of
        AtBase _ ->
          error "ship should be in flight"

        Destroyed ->
          error "ship should be in flight"

        InFlight _ destId boosted ->
          (placePoint (getPlace destId places), boosted)

    places = modelPlaces modelGame

    ships = modelShips modelGame

buildOrderText :: BuildOrder -> Text
buildOrderText = \case
  BuildPopulation -> "Population"
  BuildShield -> "Shield"
  BuildBooster -> "Booster"
  BuildShip shipType ->
    case shipType of
      Corvette -> "Corvette"
      Station -> "Station"
      Monitor -> "Monitor"

ownerColor :: Owner -> Color
ownerColor = \case
  PlayerOwner player -> playerColor player
  Neutral _          -> white

playerName :: Player -> Text
playerName = \case
  Player1 -> "Player 1"
  Player2 -> "Player 2"
