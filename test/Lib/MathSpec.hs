module Lib.MathSpec where

import Game.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "rectangleCoordinates" $ do
    it "knows when a point is outside the rectangle" $ do
      rectangleCoordinates (0,0) (Rectangle (2,2) 1 1) `shouldBe` Nothing

    it "reports a point in the rectangle correctly" $ do
      rectangleCoordinates (3,3) (Rectangle (2,2) 5 5) `shouldBe` Just (1,1)
