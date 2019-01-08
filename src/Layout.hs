module Layout
  ( Layout(..)
  , uiLayout
  , uiLayoutLookup
  , Item(..)
  , HudItem(..)
  , Clickable(..)
  , BoardItem(..)

  , sortShipsForUI
  , hasDetection
  , isFocusedBase
  , focusedBase

  , screenToUIPoint
  , screenToBoardPoint
  , sizeToRadius
  , zoomFactor
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import Game hiding (Model)
import Game.Prelude
import Model

-- | Eg:
--
-- @
-- [ { hudButton1, hudButton2 }
-- , { hudBox }
-- , { ship1, ship2, ship3 }
-- , { place1, place2 }
-- ]
-- @
newtype Layout item
  = Layout { unLayout :: [Set item] }
  deriving newtype (Semigroup, Monoid)

layoutLookup :: forall item. (item -> Bool) -> Layout item -> Maybe item
layoutLookup isItemUnderPoint =
  foldl' lkup Nothing . unLayout
  where
    lkup :: Maybe item -> Set item -> Maybe item
    lkup mAcc layerItems =
      case mAcc of
        Just item ->
          Just item

        Nothing ->
          listToMaybe (Set.toAscList (Set.filter isItemUnderPoint layerItems))

uiLayoutLookup :: Model -> ScreenPoint -> Maybe Item
uiLayoutLookup m@Model{..} clickPoint =
  layoutLookup isItemUnderPoint (uiLayout m)
  where
    isItemUnderPoint :: Item -> Bool
    isItemUnderPoint item =
      case item of
        HudItem hudItem itemScreenPoint ->
          let dimensions = case hudItem of
                             ItemHudShip _ box -> box
                             ItemBuildButton _ _ _ box -> box
                             ItemPreviousPage _ box -> box
                             ItemNextPage _ box -> box
          in withinScreenBox itemScreenPoint dimensions

        HudItself _ itemScreenPoint dimensions ->
          withinScreenBox itemScreenPoint dimensions

        BoardItem boardItem itemBoardPoint ->
          let radius = case boardItem of
                         ItemBase _ (Radius r) -> r
                         ItemShip _ (Radius r) -> r

              clickPointOnBoard :: Point
              clickPointOnBoard =
                fromBoardPoint $ screenToBoardPoint modelZoom modelPan clickPoint

          in distance clickPointOnBoard itemBoardPoint <= radius

    withinScreenBox :: ScreenPoint -> Box -> Bool
    withinScreenBox (ScreenPoint a b) box =
      let ScreenPoint x y = clickPoint
          rectangle = Rectangle (a,b) (boxWidth box) (boxHeight box)
      in case rectangleCoordinates (x,y) rectangle of
        Just _ -> True
        Nothing -> False

data Item
  = HudItem HudItem ScreenPoint
  | HudItself (Maybe PlaceId) ScreenPoint Box
    -- ^ Just is a focused base, Nothing if we're in flight.
  | BoardItem BoardItem Point
  deriving (Eq, Ord, Show)

data HudItem
  = ItemHudShip ShipId Box
  | ItemBuildButton PlaceId BuildOrder Clickable Box
  | ItemPreviousPage PlaceId Box
  | ItemNextPage PlaceId Box
  deriving (Eq, Ord, Show)

data Clickable
  = Clickable
  | NotClickable
  deriving (Eq, Ord, Show)

data BoardItem
  = ItemBase PlaceId Radius
  | ItemShip ShipId Radius
  deriving (Eq, Ord, Show)

uiLayout :: Model -> Layout Item
uiLayout m@Model{..} =
  Layout
    [ Set.map (\(hp,i) -> HudItem i (f hp)) (hudLayer m hudDimensions)
    , case modelSelection of
        SelectionNone ->
          mempty

        _ ->
          let mPlaceId :: Maybe PlaceId
              mPlaceId =
                focusedBase m -- Nothing is in flight
          in Set.singleton (HudItself mPlaceId (ScreenPoint huX huY) (Box hudWidth hudHeight))

    , Set.map (\(p,i) -> BoardItem i p) (flightLayer m)
    , Set.map (\(p,i) -> BoardItem i p) (baseLayer m)
    ]
  where
    f :: HudPoint -> ScreenPoint
    f (HudPoint x y) =
      ScreenPoint (x + huX) (y + huY)

    hudDimensions = Box hudWidth hudHeight

    Rectangle (huX, huY) hudWidth hudHeight = hudPlacement modelScreenSize

-- | First build button row.
firstButtonRowHeight :: Box -> Float
firstButtonRowHeight hudDimensions =
  boxHeight hudDimensions / 2 - 400

-- | Second build button row.
secondButtonRowHeight :: Box -> Float
secondButtonRowHeight hudDimensions =
  firstButtonRowHeight hudDimensions
    - buildButtonHeight -- one full button down
    - 15 -- margin between buttons

hudLayer :: Model -> Box -> Set (HudPoint, HudItem)
hudLayer m@Model{..} hudDimensions =
  case modelSelection of
    SelectionNone ->
      mempty

    SelectionPlace placeId ->
      hudButtonsAndShips (Just placeId) (placeShips placeId)

    SelectionShip shipId ->
      let ship = getShip shipId (modelShips modelGame)
      in case shipLocation ship of
        InFlight point _ _ ->
          let nearbyShips =
                modelShips modelGame
                  & HM.filter (\s ->
                      case shipLocation s of
                        InFlight p _ _ ->
                          distance p point < flightGroupRadius
                            && shipPlayer s == shipPlayer ship

                        AtBase _ -> False

                        Destroyed -> False)
          in hudButtonsAndShips Nothing nearbyShips

        Destroyed ->
          error "don't allow destroyed ships to be selected"

        AtBase placeId -> do
          hudButtonsAndShips (Just placeId) (placeShips placeId)
  where
    -- If a ship in flight is selected, the HUD also shows other ships
    -- which are overlapping it or very close by.
    flightGroupRadius :: Float
    flightGroupRadius =
      20

    placeShips :: PlaceId -> HashMap ShipId Ship
    placeShips placeId =
      shipsAtPlace placeId (modelShips modelGame)

    hudButtonsAndShips :: Maybe PlaceId -> HashMap ShipId Ship -> Set (HudPoint, HudItem)
    hudButtonsAndShips mPlaceId ships =
      maybe mempty hudBuildButtons mPlaceId <> hudShips m hudDimensions mPlaceId ships

    hudBuildButtons :: PlaceId -> Set (HudPoint, HudItem)
    hudBuildButtons placeId =
      case placeType place of
        Ruin ->
          mempty

        PBase base ->
          if hasDetection modelWhoAmI (Just place) (placeShips placeId)
            then
              Set.fromList $
                [ ( HudPoint (-95) row1Height
                  , ItemBuildButton placeId (BuildShip Corvette) Clickable button
                  )
                , ( HudPoint 0 row1Height
                  , ItemBuildButton placeId (BuildShip Station) Clickable button
                  )
                , ( HudPoint 95 row1Height
                  , ItemBuildButton placeId (BuildShip Monitor) Clickable button
                  )
                , ( HudPoint (-95) row2Height
                  , ItemBuildButton placeId BuildPopulation popClickable button
                  )
                , ( HudPoint 0 row2Height
                  , ItemBuildButton placeId BuildBooster (boosterClickable base) button
                  )
                , ( HudPoint 95 row2Height
                  , ItemBuildButton placeId BuildShield (shieldClickable base) button
                  )
                ]
            else
              mempty
      where
        popClickable :: Clickable
        popClickable =
          if canExpand place
            then Clickable
            else NotClickable

        shieldClickable :: Base -> Clickable
        shieldClickable base =
          if Set.member Shield (baseInstallations base)
            then NotClickable
            else Clickable

        boosterClickable :: Base -> Clickable
        boosterClickable base =
          if Set.member Booster (baseInstallations base)
            then NotClickable
            else Clickable

        place = getPlace placeId (modelPlaces modelGame)

        row1Height :: Float
        row1Height =
          firstButtonRowHeight hudDimensions

        row2Height :: Float
        row2Height =
          secondButtonRowHeight hudDimensions

        button :: Box
        button =
          Box 80 buildButtonHeight

hudShips :: Model -> Box -> Maybe PlaceId -> HashMap ShipId Ship -> Set (HudPoint, HudItem)
hudShips Model{..} hudDimensions mPlaceId ships =
  if hasDetection modelWhoAmI mPlace ships
    then
      paginatedShips <> (case mPlaceId of
                           Nothing ->
                             mempty

                           Just placeId ->
                             mPrevious placeId <> mNext placeId)
    else
      mempty
  where
    mPlace :: Maybe Place
    mPlace =
      map (\placeId -> getPlace placeId (modelPlaces modelGame)) mPlaceId

    paginatedShips :: Set (HudPoint, HudItem)
    paginatedShips =
      Set.fromList . map g $
        map (_1 +~ shipStartY) $
          mapWithIndex
            (\i ship -> (realToFrac i * negate shipHeight, ship))
            shipsOnThisPage
      where
        g :: (Float, (ShipId, Ship)) -> (HudPoint, HudItem)
        g (height, (id, _)) =
          ( HudPoint 0 height
          , ItemHudShip id (Box (boxWidth hudDimensions) shipHeight)
          )

    shipStartY :: Float
    shipStartY =
      secondButtonRowHeight hudDimensions
        - buildButtonHeight / 2 -- bottom of second button
        - 20 -- arbitrary padding
        - shipHeight / 2

    paginationRowY :: Float
    paginationRowY =
      - (boxHeight hudDimensions / 2) + paginationButtonHeight

    -- How many ships the UI has room to show
    maxShips :: Nat
    maxShips =
      floor $ abs (paginationRowY - shipStartY) / shipHeight

    pagesOfShips :: [[(ShipId, Ship)]]
    pagesOfShips =
      chunksOf (fromIntegral maxShips) (sortShipsForUI ships)

    shipsOnThisPage :: [(ShipId, Ship)]
    shipsOnThisPage =
      case pagesOfShips ^? element (fromIntegral page) of
        Nothing -> mempty
        Just xs -> xs

    page :: Nat
    page =
      case mPlaceId of
        Nothing ->
          0

        Just placeId ->
          fromMaybe 0 (HM.lookup placeId modelPlaceScroll)

    mPrevious :: PlaceId -> Set (HudPoint, HudItem)
    mPrevious placeId =
      if page == 0
        then
          mempty

        else
          Set.singleton
            ( HudPoint (-70) paginationRowY
            , ItemPreviousPage placeId (Box 80 paginationButtonHeight)
            )

    mNext :: PlaceId -> Set (HudPoint, HudItem)
    mNext placeId =
      if fromIntegral page < length pagesOfShips - 1
        then
          Set.singleton
            ( HudPoint 70 paginationRowY
            , ItemNextPage placeId (Box 80 paginationButtonHeight)
            )

        else
          mempty

sortShipsForUI :: HashMap ShipId Ship -> [(ShipId, Ship)]
sortShipsForUI ships =
  let f :: (ShipId, Ship) -> (ShipId, Ship) -> Ordering
      f (id1, s1) (id2, s2) =
        if shipType s1 == shipType s2
          then compare id1 id2
          else reverseOrdering $ compare (shipType s1) (shipType s2)
  in List.sortBy f (HM.toList ships)

-- | A player has detection at a place if they have ships
-- present or are the owner of a base there.
hasDetection
  :: Player
  -> Maybe Place
     -- ^ Place is a Maybe since we might be in flight.
  -> HashMap ShipId Ship
  -> Bool
hasDetection player mPlace ships =
  ownsBase || hasShipsThere
  where
    ownsBase :: Bool
    ownsBase =
      case mPlace of
        Nothing ->
          False

        Just place ->
          case placeType place of
            Ruin ->
              False

            PBase base ->
              PlayerOwner player == baseOwner base

    hasShipsThere :: Bool
    hasShipsThere =
      case HM.keys (HM.filter (\s -> shipPlayer s == player) ships) of
        [] ->
          False

        _ ->
          True

isFocusedBase :: Model -> PlaceId -> Bool
isFocusedBase m placeId =
  Just placeId == focusedBase m

focusedBase :: Model -> Maybe PlaceId
focusedBase Model{..} =
  case modelSelection of
    SelectionNone ->
      Nothing

    SelectionPlace placeId ->
      Just placeId

    SelectionShip shipId ->
      let ship = getShip shipId (modelShips modelGame)
      in case shipLocation ship of
        InFlight{} ->
          Nothing

        Destroyed ->
          Nothing

        AtBase placeId ->
          Just placeId

flightLayer :: Model -> Set (Point, BoardItem)
flightLayer Model{..} =
  Set.fromList . map f . hmToList $ flyingShips
  where
    flyingShips :: HashMap ShipId Point
    flyingShips =
      (\(_, loc, _) -> loc) <$> shipsInFlight (modelShips modelGame)

    f :: (ShipId, Point) -> (Point, BoardItem)
    f (id, loc) =
      (loc, ItemShip id (Radius shipClickRadius))

baseLayer :: Model -> Set (Point, BoardItem)
baseLayer Model{..} =
  Set.fromList . map f . hmToList $ places
  where
    f :: (PlaceId, Place) -> (Point, BoardItem)
    f (id, place) =
      ( placePoint place
      , ItemBase id (sizeToRadius (placeSize place))
      )

    places :: HashMap PlaceId Place
    places =
      modelPlaces modelGame

hudPlacement :: Box -> Rectangle
hudPlacement (Box screenWidth screenHeight) =
  Rectangle center hudWidth (screenHeight - margin * 2)
  where
    center :: Point
    center =
      ( (screenWidth / 2 - margin) - hudWidth / 2
      , 0
      )

    margin :: Float
    margin =
      20

    hudWidth :: Float
    hudWidth =
      300

-- | If this function seems to be broken, make sure that the view
-- is displaying things correctly on the screen.
screenToUIPoint :: Box -> Zoom -> BoardPoint -> ScreenPoint -> Either HudPoint BoardPoint
screenToUIPoint screenSize zoom pan screenPoint =
  case hudSelection of
    Just hudPoint ->
      Left hudPoint

    Nothing ->
      Right $ screenToBoardPoint zoom pan screenPoint
  where
    hudSelection :: Maybe HudPoint
    hudSelection =
      map (uncurry HudPoint) $
        rectangleCoordinates
          (fromScreenPoint screenPoint)
          (hudPlacement screenSize)

screenToBoardPoint :: Zoom -> BoardPoint -> ScreenPoint -> BoardPoint
screenToBoardPoint zoom (BoardPoint panX panY) (ScreenPoint screenX screenY) =
  BoardPoint (screenX / zf + panX) (screenY / zf + panY)
  where
    zf :: Float
    zf =
      zoomFactor zoom

sizeToRadius :: PlaceSize -> Radius
sizeToRadius size =
  Radius $
    case size of
      Large -> 65
      Medium -> 50
      Small -> 35

zoomFactor :: Zoom -> Float
zoomFactor = \case
  NoZoom   -> 1
  ZoomOut  -> 0.7
  ZoomOut2 -> 0.5
  ZoomOut3 -> 0.3
  ZoomOut4 -> 0.2

-- | We pick an arbitrary distance from the center of a ship and count
-- clicks in that radius as on that ship.
shipClickRadius :: Float
shipClickRadius =
  20

-- | The height of an entry in the ship hud list. Not all of it will
-- necessarily be filled by the ship.
shipHeight :: Float
shipHeight =
  50

buildButtonHeight :: Float
buildButtonHeight =
  40

paginationButtonHeight :: Float
paginationButtonHeight =
  30
