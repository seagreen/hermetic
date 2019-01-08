module Game.ModelSpec where

import Control.Monad.Random
import Game.Model
import Game.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified Test.QuickCheck as QuickCheck

spec :: Spec
spec = do
  describe "Gen" $ do
    modifyMaxSuccess (+ 900) $ it "is invertible through JSON" $
      QuickCheck.property $ \(n :: Int) ->
        let gen = Gen (mkStdGen n)
        in Just gen `shouldBe` decode (encode gen)
