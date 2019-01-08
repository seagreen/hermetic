module View.Ship
  ( playerColor
  , shieldColor
  , viewShip
  , viewShipWithShieldIndicator
  , viewThrust
  ) where

import Game hiding (Model)
import Game.Prelude
import Lib.Gloss

playerColor :: Player -> Color
playerColor = \case
  Player1 -> aquamarine
  Player2 -> rose

shieldColor :: Color
shieldColor =
  azure

viewShip :: Ship -> Picture
viewShip ship =
  case shipType ship of
    Corvette ->
      fold
        [ Color black $ Polygon corvetteOutline
        , Color (playerColor (shipPlayer ship)) $ lineLoop corvetteOutline
        ]

    Station ->
      fold
        [ Color black $ ThickCircle stationRadius stationRadius
        , Color (playerColor (shipPlayer ship)) $ Circle stationRadius
        ]

    Monitor ->
      fold
        [ Color black $ Polygon monitorOutline
        , Color (playerColor (shipPlayer ship)) $ lineLoop monitorOutline
        ]

stationRadius :: Float
stationRadius =
  8

-- | Ship pointed to the right.
corvetteOutline :: [Point]
corvetteOutline =
  [ (- halfLength,   halfWidth) -- Port back
  , (  halfLength,   0        ) -- Point
  , (- halfLength, - halfWidth) -- Starboard back
  ]
  where
    halfLength :: Float
    halfLength =
      15

    halfWidth :: Float
    halfWidth =
      5

-- | Ship pointed to the right.
monitorOutline :: [Point]
monitorOutline =
  [ (- halfLength,   width / 2) -- Port back
  , (  halfLength,   width * 1.5/5) -- Port front
  , (  halfLength, - width * 1.5/5) -- Starboard back
  , (- halfLength, - width / 2) -- Starboard front
  ]
  where
    halfLength :: Float
    halfLength =
      15

    width :: Float
    width =
      15

viewShipWithShieldIndicator :: Ship -> Picture
viewShipWithShieldIndicator ship =
  fold
    [ viewShip ship
    , if shipShields ship
        then
          Color shieldColor $
            case shipType ship of
              Corvette ->
                Scale 1.5 1.5 $ adjustForward $ lineLoop corvetteOutline

              Station ->
                Scale 1.5 1.5 $ Circle stationRadius

              Monitor ->
                Scale 1.5 1.5 $ lineLoop monitorOutline
        else
          mempty
    ]
  where
    -- Looks awkward otherwise
    adjustForward :: Picture -> Picture
    adjustForward =
      Translate 4 0

-- | A diamond. Long end pointed to the left.
viewThrust :: Player -> Picture
viewThrust player =
  Color (playerColor player) $ Polygon
    [ (-15,   0        ) -- Starting at the left
    , ( 10,   halfWidth) -- moving up and right
    , ( 15,   0        ) -- Far right, the point of the blunt end
    , ( 10, - halfWidth) -- moving down and left
    ]
  where
    halfWidth :: Float
    halfWidth =
      5
