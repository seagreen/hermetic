module LayoutSpec where

import Game.Prelude
import qualified Layout
import Model
import Test.Hspec

spec :: Spec
spec = do
  describe "screenToBoardPoint works" $ do
    it "when the center is clicked" $
      Layout.screenToBoardPoint NoZoom (BoardPoint 0 0) (ScreenPoint 0 0)
        `shouldBe` BoardPoint 0 0

    it "when a non-center point is clicked" $ do
      Layout.screenToBoardPoint NoZoom (BoardPoint 0 0) (ScreenPoint 2 2)
        `shouldBe` BoardPoint 2 2

    it "when zoomed out" $ do
      let zoom = ZoomOut
          zf = Layout.zoomFactor zoom

      Layout.screenToBoardPoint zoom (BoardPoint 0 0) (ScreenPoint 2 2)
        `shouldBe` BoardPoint (2 / zf) (2 / zf)

    it "when panned" $ do
      Layout.screenToBoardPoint NoZoom (BoardPoint 3 3) (ScreenPoint 2 2)
        `shouldBe` BoardPoint 5 5

    it "when panned and zoomed" $ do
      let zoom = ZoomOut
          zf = Layout.zoomFactor zoom

      Layout.screenToBoardPoint zoom (BoardPoint 3 3) (ScreenPoint 2 2)
        `shouldBe` BoardPoint (2 / zf + 3) (2 / zf + 3)
