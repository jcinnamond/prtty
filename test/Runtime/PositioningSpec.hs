module Runtime.PositioningSpec (spec) where

import Data.Vector qualified as V
import Runtime.Internal.Positioning (findCenter)
import Runtime.Internal.Types (Environment (..), emptyStyle, evalRuntime)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    centerSpec

centerSpec :: Spec
centerSpec = do
    it "returns the center of the line" $ do
        pos <- evalRuntime (findCenter 0) testEnv
        pos `shouldBe` 40

    it "offsets the line center" $ do
        pos <- evalRuntime (findCenter 14) testEnv
        pos `shouldBe` 33

testEnv :: Environment
testEnv =
    Environment
        { instructions = V.empty
        , pc = 0
        , backMarkers = []
        , lastJump = Nothing
        , height = 25
        , width = 80
        , topMargin = 0
        , leftMargin = 0
        , currentStyle = emptyStyle
        , styleHistory = []
        }