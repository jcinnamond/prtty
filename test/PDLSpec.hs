module PDLSpec (spec) where

import Data.Text (Text)
import Data.Void (Void)
import PDL (pdlParser)
import PDL qualified
import Test.Hspec (Spec, describe, it, pending)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec qualified as M

spec :: Spec
spec = do
    describe "(function) application" $ do
        it "parses (function) application" $ do
            parseExpr ".slide" `shouldParse` PDL.Instruction "slide" []
            parseExpr ".présentation" `shouldParse` PDL.Instruction "présentation" []
            parseExpr ".⏳" `shouldParse` PDL.Instruction "⏳" []
        it "consumes spaces" $ do
            parseExpr ".slide " `shouldParse` PDL.Instruction "slide" []
        it "parses sub-expressions" $ do
            parseExpr ".bold .blue" `shouldParse` PDL.Instruction "bold" [PDL.Instruction "blue" []]
            parseExpr ".bold some text\n" `shouldParse` PDL.Instruction "bold" [PDL.Literal "some text"]
        it "parses indented sub-expressions" pending

    describe "literals" $ do
        it "parses inline literals" $ do
            parseExpr "some text\n" `shouldParse` PDL.Literal "some text"
        it "parses indented literal blocks" pending

    describe "multiple expressions" $ do
        it "parses expressions over multiple lines" $ do
            let input =
                    """
                    .bold some text
                    more text
                    yet more text
                    .pause
                    """
            parse input
                `shouldParse` [ PDL.Instruction "bold" [PDL.Literal "some text"]
                              , PDL.Literal "more text"
                              , PDL.Literal "yet more text"
                              , PDL.Instruction "pause" []
                              ]

parse ::
    Text ->
    Either
        (M.ParseErrorBundle Text Void)
        [PDL.Expr]
parse = M.parse pdlParser ""

parseExpr ::
    Text ->
    Either
        (M.ParseErrorBundle Text Void)
        PDL.Expr
parseExpr = M.parse PDL.parseExpr ""