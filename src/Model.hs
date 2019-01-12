{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import Game.Model hiding (Model)
import qualified Game.Model
import Game.Prelude
import qualified Scenario.Crisis
import qualified Scenario.Tannen

data Model = Model
  { modelGame           :: Game.Model.Model
  , modelSelection      :: Selection
  , modelOrders         :: Orders
  , modelTurnEnded      :: Bool
  , modelOpponentOrders :: HashMap Player Orders
  , modelWhoAmI         :: Player
  , modelPan            :: BoardPoint
  , modelZoom           :: Zoom
  , modelPlaceScroll    :: HashMap PlaceId Natural
  , modelScreenSize     :: Box
  , modelCursorDot      :: ScreenPoint
  , modelDragToPan      :: Drag
  , modelTick           :: Tick
  , modelPopup          :: [CombatLog]
  , modelExit           :: Bool
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

init :: Gen -> Box -> Scenario -> Player -> Model
init gen screenSize scenario currentPlayer =
  Model
    { modelGame           = game
    , modelSelection      = startingSelection
    , modelOrders         = mempty
    , modelTurnEnded      = False
    , modelOpponentOrders = mempty
    , modelWhoAmI         = currentPlayer
    , modelPan            = BoardPoint 0 0
    , modelZoom           = NoZoom
    , modelPlaceScroll    = mempty
    , modelScreenSize     = screenSize
    , modelCursorDot      = ScreenPoint 10000 10000 -- off the screen to start
    , modelDragToPan      = NotDragging
    , modelTick           = Tick
    , modelPopup          = mempty
    , modelExit           = False
    }
  where
    game :: Game.Model.Model
    game =
      let fillBoard :: State Game.Model.Model ()
          fillBoard = case scenario of
                        Tannen -> Scenario.Tannen.fillBoard
                        Crisis -> Scenario.Crisis.fillBoard
      in execState fillBoard (Game.Model.init gen)

    startingSelection :: Selection
    startingSelection =
      case HM.keys (HM.filter friendly (modelPlaces game)) of
        [] -> SelectionNone
        id:_ -> SelectionPlace id
      where
        friendly :: Place -> Bool
        friendly place =
          case placeType place of
            Ruin ->
              False

            PBase base ->
              baseOwner base == PlayerOwner currentPlayer

data Scenario
  = Tannen
  | Crisis
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Selection
  = SelectionNone
  | SelectionPlace PlaceId
  | SelectionShip ShipId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data CombatLog
  = CombatLog PlaceId (Set ShipId)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | Since we only have animations that blink on and off
-- (like thursters for moving ships) we only need to track
-- two states of time.
data Tick
  = Tick
  | Tock
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data Zoom
  = NoZoom
  | ZoomOut
  | ZoomOut2
  | ZoomOut3
  | ZoomOut4
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data Drag
  = NotDragging
  | PossibleDragStart ScreenPoint
    -- ^ ScreenPoint is where the mouse was when the left button
    -- was pressed down.
    --
    -- If the user then moves the mouse past a certan threshold
    -- we switch to Dragging.
    --
    -- If they release the button before that happens
    -- they were using left click to clear their current
    -- selection, so we do that and switch to NotDragging.
  | Dragging ScreenPoint
    -- ^ ScreenPoint is the last position of the mouse.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | An unprocessed gloss point (to become a game point will need to be
-- de-panned and de-zoomed).
data ScreenPoint
  = ScreenPoint Float Float
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data HudPoint
  = HudPoint Float Float
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data BoardPoint
  = BoardPoint Float Float
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

fromScreenPoint :: ScreenPoint -> Point
fromScreenPoint (ScreenPoint x y) =
  (x,y)

fromHudPoint :: HudPoint -> Point
fromHudPoint (HudPoint x y) =
  (x,y)

fromBoardPoint :: BoardPoint -> Point
fromBoardPoint (BoardPoint x y) =
  (x,y)

maxZoom :: Zoom
maxZoom =
  NoZoom

zoomIn :: Zoom -> Zoom
zoomIn = \case
  NoZoom   -> NoZoom
  ZoomOut  -> NoZoom
  ZoomOut2 -> ZoomOut
  ZoomOut3 -> ZoomOut2
  ZoomOut4 -> ZoomOut3

zoomOut :: Zoom -> Zoom
zoomOut = \case
  NoZoom   -> ZoomOut
  ZoomOut  -> ZoomOut2
  ZoomOut2 -> ZoomOut3
  ZoomOut3 -> ZoomOut4
  ZoomOut4 -> ZoomOut4

-- * Lenses
mkLenses ''Model
