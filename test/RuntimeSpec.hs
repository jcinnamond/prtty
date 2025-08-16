module RuntimeSpec (spec) where

spec :: Spec
spec = do
    describe "SaveStyle" $ do
        it "pushes the current style onto the history" $ do
            let e = newEnvironment V.empty
            e' <- execStateT (runInstruction i) e
            e' `shouldBe` e{styleHistory = [style]}