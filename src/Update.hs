module Update where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game hiding (Model)
import Game.Prelude
import Graphics.Gloss.Interface.IO.Game
  (Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..))
import Layout
import Model

data Input
  = UserEvent Event
  | OpponentOrders Player Orders
  | TimePassed Float

update :: Input -> Model -> Model
update i m =
  case i of
    UserEvent event ->
      execState (userInput event) m

    TimePassed _ ->
      tickOrTock m

    OpponentOrders p o ->
      execState checkForNewTurn (m & modelOpponentOrdersL %~ HM.insert p o)

--------------------------------------------------------------------------------
-- * Time passed
--------------------------------------------------------------------------------

tickOrTock :: Model -> Model
tickOrTock =
  modelTickL %~ next
  where
    next :: Tick -> Tick
    next = \case
      Tick -> Tock
      Tock -> Tick

--------------------------------------------------------------------------------
-- * checkForNewTurn
--
-- Called after we receive opponent orders or end our own turn.
--------------------------------------------------------------------------------

checkForNewTurn :: State Model ()
checkForNewTurn = do
  b <- isTurnOver
  when b updateTurnEnd
  where
    isTurnOver :: State Model Bool
    isTurnOver = do
      playerTurnEnded <- use modelTurnEndedL
      opOrders <- use modelOpponentOrdersL
      pure $ playerTurnEnded && HM.size opOrders > 0

updateTurnEnd :: State Model ()
updateTurnEnd = do
  currentPlayer <- use modelWhoAmIL
  allOrders <- do
    playerOrders <- use modelOrdersL
    opponentOrders <- use modelOpponentOrdersL
    pure $ opponentOrders <> HM.singleton currentPlayer playerOrders

  modelGameL %= Game.update allOrders

  lastTurnLog <- use (modelGameL . modelLogL)
  modelPopupL .= map (uncurry CombatLog) (hmToList (logCombat lastTurnLog))

  modelOrdersL .= mempty
  modelOpponentOrdersL .= mempty
  resetPaginations
  perhapsClearSelection
  modelTurnEndedL .= False

resetPaginations :: State Model ()
resetPaginations =
  modelPlaceScrollL .= mempty

perhapsClearSelection :: State Model ()
perhapsClearSelection = do
  currentPlayer <- use modelWhoAmIL
  ships <- use (modelGameL . modelShipsL)
  selected <- use modelSelectionL
  case selected of
    SelectionNone ->
      pure ()

    SelectionPlace _ ->
      pure ()

    SelectionShip shipId -> do
      let ship = getShip shipId ships
      case shipLocation ship of
        Destroyed ->
          modelSelectionL .= SelectionNone

        InFlight{} ->
          when (shipPlayer ship /= currentPlayer) $
            modelSelectionL .= SelectionNone

        AtBase _ ->
          pure ()

--------------------------------------------------------------------------------
-- * Result check
--------------------------------------------------------------------------------

data UpdateResult
  = Normal
  | PlayerEndedTurn Orders
  | Exit

updateResult :: Model -> Model -> UpdateResult
updateResult oldModel m
  | modelExit m = Exit
  | playerEndedTurn = PlayerEndedTurn (modelOrders oldModel)
  | otherwise = Normal
  where
    playerEndedTurn :: Bool
    playerEndedTurn =
         (not (modelTurnEnded oldModel) && modelTurnEnded m)
      || turnRolledForward

    turnRolledForward :: Bool
    turnRolledForward =
      let f = modelTurn . modelGame
      in f oldModel /= f m

--------------------------------------------------------------------------------
-- * User input
--------------------------------------------------------------------------------

userInput :: Event -> State Model ()
userInput event =
  case event of
    EventKey (SpecialKey KeyEsc) Down _ _ ->
      modelExitL .= True

    _ -> do
      model <- get
      case modelPopup model of
        [] ->
          updateNormal event

        popup:rest ->
          updatePopup event popup rest

updatePopup :: Event -> CombatLog -> [CombatLog] -> State Model ()
updatePopup event _ rest = do
  case event of
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      modelPopupL .= rest

    _ ->
      pure ()

updateNormal :: Event -> State Model ()
updateNormal event = do
  model <- get
  let currentPlayer = modelWhoAmI model
      places = modelPlaces (modelGame model)
      ships = modelShips (modelGame model)

      whenCanMove :: State Model () -> State Model ()
      whenCanMove move =
        case outcome (modelGame model) of
          Victor _ ->
            pure ()

          AllDefeated ->
            pure ()

          Ongoing ->
            when (not (modelTurnEnded model)) move

  case event of
    EventMotion (x,y) -> do
      let
        screenPoint :: ScreenPoint
        screenPoint =
          ScreenPoint x y

      modelCursorDotL .= screenPoint

      dragToPan <- use modelDragToPanL
      case dragToPan of
        NotDragging ->
          pure ()

        PossibleDragStart initial@(ScreenPoint initialX initialY) -> do
          if distance (x,y) (initialX,initialY) < 10
            then
              pure ()

            else do
              modelDragToPanL .= Dragging initial

        Dragging old -> do
          BoardPoint oldX oldY <- runScreenToBoardPoint old
          BoardPoint newX newY <- runScreenToBoardPoint screenPoint
          let f :: BoardPoint -> BoardPoint
              f (BoardPoint panX panY) =
                BoardPoint (panX + (oldX - newX)) (panY + (oldY - newY))
          modelPanL %= f
          modelDragToPanL .= Dragging screenPoint

    EventResize (width, height) ->
      modelScreenSizeL .= Box (realToFrac width) (realToFrac height)

    -- The meaning of latestMouseX, latestMouseY needs to be documented in gloss.
    EventKey key upOrDown _ (latestMouseX, latestMouseY) -> do
      let
        screenPoint :: ScreenPoint
        screenPoint =
          ScreenPoint latestMouseX latestMouseY

      case upOrDown of
        Up ->
          case key of
            MouseButton LeftButton -> do
              dragToPan <- use modelDragToPanL
              case dragToPan of
                -- We didn't move very much while we had the mouse down
                -- so we weren't dragging at all! Instead we were
                -- left clicking to clear the selection.
                PossibleDragStart _ ->
                  modelSelectionL .= SelectionNone

                NotDragging ->
                  pure ()

                Dragging _ ->
                  pure ()

              modelDragToPanL .= NotDragging

            _ ->
              pure ()

        Down ->
          case key of
            Char 'c' ->
              modelPanL .= BoardPoint 0 0

            Char '=' -> do
              currentZoom <- use modelZoomL
              when (currentZoom /= maxZoom) (panTowardsCursor screenPoint)

              modelZoomL %= zoomIn

            Char '-' ->
              modelZoomL %= zoomOut

            Char _ ->
              pure ()

            SpecialKey KeySpace ->
              whenCanMove $ do
                modelTurnEndedL .= True
                checkForNewTurn

            SpecialKey _ ->
              pure ()

            MouseButton WheelUp -> do
              currentZoom <- use modelZoomL
              when (currentZoom /= maxZoom) (panTowardsCursor screenPoint)

              modelZoomL %= zoomIn

            MouseButton WheelDown ->
              modelZoomL %= zoomOut

            MouseButton button -> do
              case interpretMouseMsg model screenPoint button of
                BaseSelect placeId ->
                  modelSelectionL .= SelectionPlace placeId

                SwitchBuilding placeId buildOrder ->
                  whenCanMove $ do
                    let place = getPlace placeId places
                    case placeType place of
                      Ruin ->
                        pure ()

                      PBase base ->
                        when (baseOwner base == PlayerOwner currentPlayer) $ do
                          if baseBuilding base == buildOrder
                            then
                              modelOrdersL . ordersBuildL %= HM.delete placeId
                            else
                              modelOrdersL . ordersBuildL %= HM.insert placeId buildOrder

                ShipSelect shipId ->
                  modelSelectionL .= SelectionShip shipId

                ShipsEmbark shipIds _ destId ->
                  whenCanMove $
                    for_ shipIds $ \shipId -> do
                      let ship = getShip shipId ships
                      when (shipPlayer ship == currentPlayer) $
                        modelOrdersL . ordersEmbarkL %= HM.insert shipId destId

                PreviousPage placeId ->
                  modelPlaceScrollL %= HM.adjust (subtract 1) placeId

                NextPage placeId ->
                  modelPlaceScrollL %= HM.insertWith (+) placeId 1

                EmptySpace ->
                  modelDragToPanL .= PossibleDragStart screenPoint

                NoOp ->
                  pure ()

-- | Do this softly so zooming in doesn't fling us away from the map.
--
-- This calculation isn't needed by View so no need to move it into Msg.
panTowardsCursor :: ScreenPoint -> State Model ()
panTowardsCursor screenPoint = do
  uiPoint <- runScreenToUIPoint screenPoint
  case uiPoint of
    Left _ -> pure ()
    Right selectedPoint ->
      modelPanL %= panTowards selectedPoint
  where
    panTowards :: BoardPoint -> BoardPoint -> BoardPoint
    panTowards selectedPoint panPoint@(BoardPoint px py) =
      let
        sel = fromBoardPoint selectedPoint
        pan = fromBoardPoint panPoint
        d = distance sel pan
        speed = d / 3
        (x,y) = deltas (angleBetweenPoints pan sel) speed
      in BoardPoint (px + x) (py + y)

runScreenToUIPoint :: ScreenPoint -> State Model (Either HudPoint BoardPoint)
runScreenToUIPoint screenPoint = do
  screenSize <- use modelScreenSizeL
  pan <- use modelPanL
  zoom <- use modelZoomL
  pure (screenToUIPoint screenSize zoom pan screenPoint)

runScreenToBoardPoint :: ScreenPoint -> State Model BoardPoint
runScreenToBoardPoint screenPoint = do
  pan <- use modelPanL
  zoom <- use modelZoomL
  pure (screenToBoardPoint zoom pan screenPoint)

data Msg
  = BaseSelect PlaceId
  | SwitchBuilding PlaceId BuildOrder
  | ShipSelect ShipId
  | ShipsEmbark (Set ShipId) PlaceId PlaceId -- ^ ships, departure, destination
  | PreviousPage PlaceId
  | NextPage PlaceId
  | EmptySpace
  | NoOp

interpretMouseMsg :: Model -> ScreenPoint -> MouseButton -> Msg
interpretMouseMsg m@Model{..} screenPoint button = do
  case button of
    LeftButton ->
      handleLeftButton

    RightButton ->
      fromMaybe NoOp handleRightButton

    _ ->
      NoOp

  where
    mChosenItem :: Maybe Item
    mChosenItem =
      uiLayoutLookup m screenPoint

    handleLeftButton :: Msg
    handleLeftButton =
      case mChosenItem of
        Nothing ->
          EmptySpace

        Just chosenItem ->
          case chosenItem of
            HudItem item _ ->
              case item of
                ItemHudShip id _ ->
                  ShipSelect id

                ItemBuildButton placeId buildOrder clickable _ ->
                  case clickable of
                    NotClickable ->
                      NoOp

                    Clickable ->
                      SwitchBuilding placeId buildOrder

                ItemPreviousPage placeId _ ->
                  PreviousPage placeId

                ItemNextPage placeId _ ->
                  NextPage placeId

            HudItself mPlaceId _ _ ->
              maybe NoOp BaseSelect mPlaceId

            BoardItem item _ ->
              case item of
                ItemBase id _ ->
                  BaseSelect id

                ItemShip id _ ->
                  ShipSelect id

    handleRightButton :: Maybe Msg
    handleRightButton = do
      chosenItem <- mChosenItem
      case chosenItem of
        BoardItem (ItemBase chosenId _) _ -> do
          case modelSelection of
            SelectionNone ->
              Nothing

            SelectionPlace placeId ->
              let ships = shipsAtPlace placeId (modelShips modelGame)
                  shipsLessStations = HM.filter (\s -> shipType s /= Station) ships
                  ids = Set.fromList (HM.keys shipsLessStations)
              in Just (ShipsEmbark ids placeId chosenId)

            SelectionShip shipId ->
              let ship = getShip shipId (modelShips modelGame)
              in case shipLocation ship of
                InFlight{} ->
                  Nothing

                Destroyed ->
                  Nothing

                AtBase departureId ->
                  Just (ShipsEmbark (Set.singleton shipId) departureId chosenId)

        _ ->
          Nothing
