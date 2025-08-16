module Runtime.NavigationSpec (spec) where

import Control.Monad.State (execStateT)
import Data.Vector qualified as V
import Runtime.Internal.Navigation (goBack, jump, jumpToMarker, storeBackMarker)
import Runtime.Internal.Types (Environment (..), Instruction (..), emptyStyle)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    jumpSpec
    jumpToMarkerSpec
    goBackSpec
    setBackMarkerSpec

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

goBackSpec :: Spec
goBackSpec = do
    it "jumps to after the previous back marker" $ do
        let is = V.fromList [Output "hi", StoreBackMarker, Output "bye", WaitForInput]
        env' <- execStateT goBack testEnv{instructions = is, pc = 4}
        env'.pc `shouldBe` 2
        env'.lastJump `shouldBe` Just 1

    it "jumps past the last jump" $ do
        let is = V.fromList [StoreBackMarker, Output "hi", StoreBackMarker, Output "bye", WaitForInput]
        env' <- execStateT goBack testEnv{instructions = is, pc = 5, lastJump = Just 2}
        env'.pc `shouldBe` 1
        env'.lastJump `shouldBe` Just 0

    it "jumps to the start if there are no previous markers" $ do
        let is = V.fromList [Output "hi", Output "bye", StoreBackMarker, WaitForInput]
        env' <- execStateT goBack testEnv{instructions = is, pc = 1}
        env'.pc `shouldBe` 0
        env'.lastJump `shouldBe` Just 0

    it "jumps to the start if the only previous marker had been jumped to" $ do
        let is = V.fromList [Output "hi", StoreBackMarker, Output "bye", WaitForInput]
        env' <- execStateT goBack testEnv{instructions = is, pc = 4, lastJump = Just 1}
        env'.pc `shouldBe` 0
        env'.lastJump `shouldBe` Just 0

    it "doesn't move when at the start of the presentation" $ do
        let is = V.fromList [Output "hi", StoreBackMarker, Output "bye", WaitForInput]
            env = testEnv{instructions = is}
        env' <- execStateT goBack env
        env' `shouldBe` env{lastJump = Just 0}

setBackMarkerSpec :: Spec
setBackMarkerSpec = do
    it "clears any previous jump" $ do
        let env = testEnv{lastJump = Just 2}
        env' <- execStateT storeBackMarker env
        env' `shouldBe` env{lastJump = Nothing}

    it "does nothing if no previous jump has been stored" $ do
        env' <- execStateT storeBackMarker testEnv
        env' `shouldBe` testEnv

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