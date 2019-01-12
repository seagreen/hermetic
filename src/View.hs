module View
  ( view
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Game hiding (Model)
import Game.Prelude
import Layout
import Lib.Gloss
import Model
import View.Board
import View.Hud
import View.Ship

view :: Model -> Picture
view m@Model{..} =
  fold
    [ clickableItems
    , topLeft modelScreenSize (viewStatus m)
    , viewCursor m
    , viewPopup m modelPopup
    , viewTurnEndedNotice m
    , viewOutcomeNotice m
    ]
  where
    clickableItems :: Picture
    clickableItems =
      map viewLayoutItems (unLayout (uiLayout m))
        & reverse -- Gloss's Picture monoid layers bottom-to-top, not top-to-bottom
        & fold

    viewLayoutItems :: Set Item -> Picture
    viewLayoutItems =
      fold . map (viewItem m) . Set.toList

viewItem :: Model -> Item -> Picture
viewItem m@Model{..} = \case
  HudItem item (ScreenPoint x y) ->
    Translate x y $
      case item of
        ItemHudShip id box ->
          viewHudShip m id box

        ItemBuildButton _ buildOrder clickable box ->
          viewBuildButton buildOrder clickable box

        ItemPreviousPage _ box ->
          previousButton box

        ItemNextPage _ box ->
          nextButton box

  HudItself _ (ScreenPoint x y) hudDimensions ->
    if modelSelection == SelectionNone
      then mempty
      else Translate x y $ viewHud m hudDimensions

  BoardItem item (x,y) ->
    panAndZoom m . Translate x y $
      case item of
        ItemBase id radius -> viewPlace m id radius
        ItemShip id radius -> viewShipInFlight m id (x,y) radius

previousButton :: Box -> Picture
previousButton box =
  fold
    [ whiteOnBlackBox box
    , centeredText white "Previous"
    ]

nextButton :: Box -> Picture
nextButton box =
  fold
    [ whiteOnBlackBox box
    , centeredText white "Next"
    ]

viewPopup :: Model -> [CombatLog] -> Picture
viewPopup m = \case
  [] ->
    mempty

  popup:_ ->
    viewSinglePopup m popup

viewSinglePopup :: Model -> CombatLog -> Picture
viewSinglePopup Model{..} (CombatLog id ships) =
  fold
    [ whiteOnBlackBox (Box 600 800)
    , Translate 0 280 $ Scale 2 2 $ centeredText white ("Combat at " <> placeName (getPlace id places))
    , case Set.toList ships of
        [] ->
          mempty

        _ ->
          Scale 1.2 1.2 $ Translate 0 180 $ fold
            [ centeredText white "Destroyed"
            , Translate (-14) (-10) $ centeredText white "-------"
            , Translate 0 (-40) $ centeredText white destroyedMsg
            ]

    , Translate 0 (-300) $
        centeredText white "<ENTER> to dismiss"
    ]
  where
    places = modelPlaces modelGame

    destroyedMsg :: Text
    destroyedMsg =
      T.intercalate ", " $ map destroyedName $ Set.toList ships

    destroyedName :: ShipId -> Text
    destroyedName shipId =
      case shipPlayer (getShip shipId (modelShips modelGame)) of
        Player1 -> "Player 1 ship"
        Player2 -> "Player 2 ship"

viewTurnEndedNotice :: Model -> Picture
viewTurnEndedNotice Model{..} =
  case outcome modelGame of
    Victor victor ->
      if modelWhoAmI == victor
        then
          fold
            [ coloredBox green (Box 400 100)
            , centeredText white "Victory"
            ]
        else
          defeat

    AllDefeated ->
      defeat

    Ongoing ->
      mempty
  where
    defeat :: Picture
    defeat =
      fold
        [ coloredBox red (Box 400 100)
        , centeredText white "Defeat"
        ]

viewOutcomeNotice :: Model -> Picture
viewOutcomeNotice Model{..} =
  if modelTurnEnded
   then
     fold
       [ whiteOnBlackBox (Box 400 100)
       , centeredText white "Waiting for opponent"
       ]
   else
     mempty

viewStatus :: Model -> Picture
viewStatus Model{modelGame, modelWhoAmI} =
  verticalConcatText
    [ viewPlayerName modelWhoAmI
    , viewTurn (modelTurn modelGame)
    , viewText white "[c] to center"
    ]

viewPlayerName :: Player -> Picture
viewPlayerName player =
  viewText (playerColor player) (playerName player)

viewTurn :: Natural -> Picture
viewTurn turn =
  viewText white ("Turn: " <> T.pack (show turn) <> " (<SPACEBAR> to end turn)")

topLeft :: Box -> Picture -> Picture
topLeft (Box screenWidth screenHeight) =
  Translate 20 (-40) . Translate (-x) y
  where
    x :: Float
    x =
      screenWidth / 2

    y :: Float
    y =
      screenHeight / 2

viewCursor :: Model -> Picture
viewCursor m@Model{..} =
  Translate mouseX mouseY $ color $ circleSolid 3
  where
    ScreenPoint mouseX mouseY = modelCursorDot

    color :: Picture -> Picture
    color =
      case uiLayoutLookup m modelCursorDot of
        Nothing -> Color (greyN 0.5)
        Just item ->
          case item of
            HudItem{} -> identity
            HudItself{} -> identity
            BoardItem ItemBase{} _ -> Color red
            BoardItem{} -> Color (greyN 0.5)

panAndZoom :: Model -> Picture -> Picture
panAndZoom Model{modelPan, modelZoom} =
  scaleZoom . translatePan -- The order (pan, then zoom) matters here.
  where
    translatePan :: Picture -> Picture
    translatePan =
      let BoardPoint x y = modelPan
      in Translate (negate x) (negate y)

    scaleZoom :: Picture -> Picture
    scaleZoom =
      let zf = zoomFactor modelZoom
      in Scale zf zf
