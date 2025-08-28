module Runtime.PositioningSpec (spec) where

import Runtime.Helpers (testEnv)
import Runtime.Internal.Positioning (Position (..), posFromHome, xFromCenter, yFromCenter)
import Runtime.Internal.Types (Environment (leftMargin, topMargin), evalRuntime)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    xFromCenterSpec
    yFromCenterSpec
    posFromHomeSpec

xFromCenterSpec :: Spec
xFromCenterSpec = do
    it "returns the center of the line" $ do
        x <- evalRuntime (xFromCenter 0) testEnv
        x `shouldBe` 40

    it "offsets the line center" $ do
        x <- evalRuntime (xFromCenter 14) testEnv
        x `shouldBe` 33

yFromCenterSpec :: Spec
yFromCenterSpec = do
    it "returns the center of the screen" $ do
        y <- evalRuntime (yFromCenter 0) testEnv
        y `shouldBe` 12

    it "offsets the center" $ do
        y <- evalRuntime (yFromCenter 5) testEnv
        y `shouldBe` 10

posFromHomeSpec :: Spec
posFromHomeSpec = do
    it "returns the top left, offset by margins" $ do
        pos <- evalRuntime (posFromHome) testEnv{leftMargin = 2, topMargin = 3}
        pos `shouldBe` Position{x = 2, y = 3}