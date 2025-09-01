module Runtime.PositioningSpec (spec) where

import Runtime.Internal.Positioning (resolve)
import Runtime.Internal.Types (Anchor (..))
import Runtime.Value (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    resolveSpec

resolveSpec :: Spec
resolveSpec = do
    describe "from top left" $ do
        it "returns the numerical values" $
            resolve (Number 3) TopLeft 25 0 `shouldBe` 3
