module Lib.Gloss
  ( module Lib.Gloss
  , module Graphics.Gloss.Data.Color
    -- * Re-exports from "Graphics.Gloss.Data.Picture"
    --
    -- | /(With redunant aliases for Picture constructors hidden)/
  , module Graphics.Gloss.Data.Picture
  ) where

import qualified Data.Text as T
import Game.Prelude
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding
  (arc, bitmap, bitmapSection, blank, circle, color, line, pictures, polygon,
  rotate, scale, text, thickArc, thickCircle, translate)

whiteOnBlackBox :: Box -> Picture
whiteOnBlackBox =
  coloredBox white

coloredBox :: Color -> Box -> Picture
coloredBox color (Box w h) =
  fold
    [ Color black $ rectangleSolid w h
    , Color color $ rectangleWire w h
    ]

-- | Horizontal centering is just an approximation since the text isn't fixed width.
centeredText :: Color -> Text -> Picture
centeredText color t =
  Translate x 0 $ verticallyCenteredText color t
  where
    x :: Float
    x =
      (realToFrac (T.length t) / 2) * (-8)

verticallyCenteredText :: Color -> Text -> Picture
verticallyCenteredText color =
  Translate 0 y . viewText color
  where
    y :: Float
    y =
      -5

viewText :: Color -> Text -> Picture
viewText color =
  Scale (1/8) (1/8) . Color color . Text . T.unpack

verticalConcatText :: [Picture] -> Picture
verticalConcatText =
  snd . foldl' f (0,mempty)
  where
    f :: (Float, Picture) -> Picture -> (Float, Picture)
    f (translateBy, currentPicture) textToAdd =
      case textToAdd of
        Blank ->
          (translateBy, currentPicture)

        _ ->
          ( translateBy - 20
          , currentPicture <> Translate 0 translateBy textToAdd
          )
