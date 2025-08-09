module Compiler.Helpers where

import Compiler.Internal.Types (Compiler', evalCompiler)
import Data.Text (Text)
import Test.Hspec (Expectation, shouldBe)

shouldCompileTo :: (Show a, Eq a) => Compiler' a -> a -> Expectation
shouldCompileTo c es = evalCompiler c `shouldBe` Right es

shouldThrowError :: (Show a, Eq a) => Compiler' a -> Text -> Expectation
shouldThrowError c err = evalCompiler c `shouldBe` Left err