module Runtime.PositioningSpec (spec) where

import Runtime.Helpers (testEnv)
import Runtime.Internal.Positioning (resolve, setLeftMargin, setTopMargin)
import Runtime.Internal.Types (Anchor (..), Environment (..), execRuntime)
import Runtime.Value (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    resolveSpec
    setMarginSpec

resolveSpec :: Spec
resolveSpec = do
    describe "resolve" $ do
        describe "from top left" $ do
            it "returns numerical values" $
                resolve (Number 3) TopLeft 25 0 `shouldBe` 3
            it "multiplies by rational numbers" $
                resolve (Rational 2 25) TopLeft 25 0 `shouldBe` 2
            it "calculates percentages" $
                resolve (Percentage 50) TopLeft 25 0 `shouldBe` 12

        describe "from bottom right" $ do
            it "returns numerical values, offset from the limit" $
                resolve (Number 3) BottomRight 25 0 `shouldBe` 22
            it "multiplies by rational numbers, offset from the limit" $
                resolve (Rational 2 25) BottomRight 25 0 `shouldBe` 23
            it "calculates precentages" $
                resolve (Percentage 50) BottomRight 25 0 `shouldBe` 13

        describe "from the margin" $ do
            it "returns numerical values, offset by the margin" $
                resolve (Number 3) Margin 25 5 `shouldBe` 8
            it "multiplies by rational numbers, offset by the margin" $ do
                resolve (Rational 2 10) Margin 25 5 `shouldBe` 8
            it "calcultes percentages, offset by the margin" $ do
                resolve (Percentage 50) Margin 25 5 `shouldBe` 12

setMarginSpec :: Spec
setMarginSpec = do
    describe "setTopMargin" $ do
        it "sets an absolute margin" $ do
            env' <- execRuntime (setTopMargin $ Number 10) testEnv
            env'.topMargin `shouldBe` 10
        it "calculates the margin from a rational" $ do
            env' <- execRuntime (setTopMargin $ Rational 1 3) testEnv{height = 30}
            env'.topMargin `shouldBe` 10
        it "calculates the margin from a percentage" $ do
            env' <- execRuntime (setTopMargin $ Percentage 50) testEnv{height = 30}
            env'.topMargin `shouldBe` 15

    describe "setLeftMargin" $ do
        it "sets an absolute margin" $ do
            env' <- execRuntime (setLeftMargin $ Number 10) testEnv
            env'.leftMargin `shouldBe` 10
        it "calculates the margin from a rational" $ do
            env' <- execRuntime (setLeftMargin $ Rational 1 3) testEnv{width = 30}
            env'.leftMargin `shouldBe` 10
        it "calculates the margin from a percentage" $ do
            env' <- execRuntime (setLeftMargin $ Percentage 50) testEnv{width = 30}
            env'.leftMargin `shouldBe` 15
