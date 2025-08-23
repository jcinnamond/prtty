module Runtime.NavigationSpec (spec) where

import Control.Monad.State (execStateT)
import Data.Vector qualified as V
import Runtime.Instructions (Instruction (..))
import Runtime.Internal.Navigation (jump, jumpToMarker)
import Runtime.Internal.Types (Environment (..), emptyStyle)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    jumpSpec
    jumpToMarkerSpec

jumpSpec :: Spec
jumpSpec = do
    it "sets the next instruction address" $ do
        env' <- execStateT (jump 10) testEnv
        env'.pc `shouldBe` 10

jumpToMarkerSpec :: Spec
jumpToMarkerSpec = do
    it "sets the next instruction to the marker" $ do
        let is = V.fromList [Output "hi", Newline, SetMarker "marker", Output "bye"]
        env' <- execStateT (jumpToMarker "marker") testEnv{instructions = is}
        env'.pc `shouldBe` 2

    it "doesn't modify the env if the marker can't be found" $ do
        let is = V.fromList [Output "hi", Newline, SetMarker "marker", Output "bye"]
            env = testEnv{instructions = is}
        env' <- execStateT (jumpToMarker "wrong") env
        env' `shouldBe` env

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