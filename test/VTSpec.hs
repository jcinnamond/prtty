module VTSpec (spec) where

import Test.Hspec(Spec, it, shouldBe)
import VT

spec :: Spec
spec = do
    it "formats literal text" $ do
        toText "hi" `shouldBe` "hi"
    it "combines text literals" $ do
        toText ("hi" <> "there") `shouldBe` "hithere"

    it "formats single attributes" $ do
        toText (Style "1") `shouldBe` "\x1b[1m"
    it "formats multiple attributes" $ do
        toText (Style "1" <> Style "2") `shouldBe` "\x1b[1;2m"

    it "formats colors" $ do
        toText (fgColor (0x77, 0x77, 0xff)) `shouldBe` "\x1b[38;2;119;119;255m"
        toText (bgColor (0x77, 0x77, 0xff)) `shouldBe` "\x1b[48;2;119;119;255m"

    it "formats reset" $ do
        toText reset `shouldBe` "\x1b[0m"

    it "formats combined attributes and text" $ do
        toText (Style "1" <> "hi" <> reset) `shouldBe` "\x1b[1mhi\x1b[0m"
    it "formats multiple attributes combined with text" $ do
        toText ((Style "1" <> Style "2") <> "hi" <> reset) `shouldBe` "\x1b[1;2mhi\x1b[0m"
