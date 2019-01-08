module View.Hud where

import qualified Data.HashMap.Strict as HM
import Game hiding (Model)
import Game.Prelude
import Layout
import Lib.Gloss
import Model
import View.Board
import View.Ship

viewHud :: Model -> Box -> Picture
viewHud m@Model{..} hudDimensions@(Box width height) =
  fold
    [ whiteOnBlackBox hudDimensions
    , Translate 0 (height / 2 - 30) baseNameHeader
    , Translate 0 (height / 2 - 180) photo
    , case mIdPlace of
        Nothing -> -- We're in flight
          mempty

        Just (placeId, place) ->
          case placeType place of
            Ruin ->
              mempty

            PBase base ->
              if baseOwner base == PlayerOwner modelWhoAmI
                then
                  Translate 0 (height / 2 - 345) $
                    Scale 1.2 1.2 $
                      centeredText white (buildingText modelOrders placeId base)
                else
                  mempty
    ]
  where
  -- Are we in flight?
  mIdPlace :: Maybe (PlaceId, Place)
  mIdPlace = do
    placeId <- focusedBase m
    Just (placeId, getPlace placeId (modelPlaces modelGame))

  baseNameHeader :: Picture
  baseNameHeader =
    Scale 2 2 $
      case mIdPlace of
        Nothing -> centeredText white "<In flight>"
        Just (_, place) ->
          case placeType place of
            Ruin ->
              centeredText white ("Ruins of " <> placeName place)

            PBase base ->
              centeredText (ownerColor (baseOwner base)) (placeName place)

  photo :: Picture
  photo =
    fold
      [ Color black $ rectangleSolid w h
      , Color white $ rectangleWire w h
      , case mIdPlace of
          Nothing -> mempty
          Just (_, place) ->
            Scale 1.2 1.2 $
              case placeType place of
                Ruin ->
                  ruinPicture

                PBase base ->
                  viewBasePhoto base (sizeToRadius (placeSize place))
      ]
    where
      w :: Float
      w =
        width - 50

      h :: Float
      h =
        w

viewHudShip :: Model -> ShipId -> Box -> Picture
viewHudShip Model{..} shipId (Box width height) =
  fold
    [ Scale 2 2 $ viewShipWithShieldIndicator ship
    , Translate 100 (-10) destination
    , if isFocused
        then Color yellow $ rectangleWire width height
        else mempty
    ]
  where
    ship :: Ship
    ship =
      getShip shipId (modelShips modelGame)

    isFocused :: Bool
    isFocused =
      let focusedDueToBaseSelection =
            case shipLocation ship of
              InFlight{} ->
                False

              Destroyed ->
                False

              AtBase placeId ->
                SelectionPlace placeId == modelSelection
                  && shipPlayer ship == modelWhoAmI
                  -- Station's don't all move along with other ships
                  -- because they're slow slow this will rarely be what
                  -- the player wants:
                  && shipType ship /= Station

      in SelectionShip shipId == modelSelection
           || focusedDueToBaseSelection

    destination :: Picture
    destination =
      let showDest :: PlaceId -> Picture
          showDest placeId =
            centeredText white (placeName (getPlace placeId places))

      in case shipLocation ship of
        InFlight _ dest _ ->
          showDest dest

        AtBase _ ->
          case HM.lookup shipId (ordersEmbark modelOrders) of
            Nothing ->
              mempty

            Just placeId ->
              showDest placeId

        Destroyed ->
          mempty

    places = modelPlaces modelGame

viewBuildButton :: BuildOrder -> Clickable -> Box -> Picture
viewBuildButton buildOrder clickable box =
  fold
    [ case clickable of
        NotClickable ->
          mempty

        Clickable ->
          whiteOnBlackBox box

    , centeredText white (buildOrderText buildOrder)
    ]
