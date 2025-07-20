{-# LANGUAGE QuasiQuotes #-}

module PDLSpec (spec) where

import PDL.Internal qualified as PDL
import PDL.Types qualified as PDL
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec qualified as M
import Text.RawString.QQ (r)

spec :: Spec
spec = do
    describe "parseIdentifier" $ do
        let parse = M.parse PDL.parseIdentifier ""
        it "parses alphanums and symbols" $ do
            parse "clear" `shouldParse` "clear"
            parse "présentation" `shouldParse` "présentation"
            parse "⏳" `shouldParse` "⏳"
        it "does not consume spaces" $ do
            parse "clear " `shouldParse` "clear"

    describe "parseHex" $ do
        let parse = M.parse PDL.parseHex ""
        it "parses hex values" $ do
            parse "#11aae5" `shouldParse` PDL.Hex ('1', '1') ('a', 'a') ('e', '5')

    describe "parseDuration" $ do
        let parse = M.parse PDL.parseDuration ""
        it "parses seconds" $ do
            parse "10s" `shouldParse` PDL.Seconds 10
        it "parses milliseconds" $ do
            parse "100ms" `shouldParse` PDL.Milliseconds 100

    describe "parseTextLiteral" $ do
        let parse = M.parse PDL.parseTextLiteral ""
        it "fails on empty input" $ do
            parse `shouldFailOn` ""

        it "parses quoted strings" $ do
            parse [r|"hi"|] `shouldParse` "hi"
            parse
                """
                \"text
                over
                multiple lines\"
                """
                `shouldParse` "text\nover\nmultiple lines"
            parse [r|"it'll work with apostrophes"|] `shouldParse` "it'll work with apostrophes"

        it "parses escaped quotes" $ do
            parse [r|"hi\"there"|] `shouldParse` "hi\"there"

        it "parses newlines" $ do
            parse [r|"some\ntext"|] `shouldParse` "some\ntext"

    describe "parseArgs" $ do
        let parse = M.parse PDL.parseArgs ""
        it "parses a single hex argument" $ do
            parse "[#112233]" `shouldParse` PDL.Args [PDL.ArgHex (PDL.Hex ('1', '1') ('2', '2') ('3', '3'))]
        it "parses a single duration argument" $ do
            parse "[2s]" `shouldParse` PDL.Args [PDL.ArgDuration $ PDL.Seconds 2]
        it "parses a single text argument" $ do
            parse [r|["blue"]|] `shouldParse` PDL.Args [PDL.ArgText "blue"]

        it "parses multiple arguments" $ do
            parse [r|["blue"; "bold"; 50ms]|]
                `shouldParse` PDL.Args
                    [ PDL.ArgText "blue"
                    , PDL.ArgText "bold"
                    , PDL.ArgDuration $ PDL.Milliseconds 50
                    ]
        it "skips whitespace" $ do
            parse [r|[ "blue" ;"bold";   50ms   ]|]
                `shouldParse` PDL.Args
                    [ PDL.ArgText "blue"
                    , PDL.ArgText "bold"
                    , PDL.ArgDuration $ PDL.Milliseconds 50
                    ]

        it "parses empty args" $ do
            parse "[]" `shouldParse` PDL.Args []

        it "defaults to empty args if not provided" $ do
            parse "these are not args" `shouldParse` PDL.Args []
            parse "" `shouldParse` PDL.Args []

    describe "parseInstruction" $ do
        let parse = M.parse PDL.parseInstruction ""
        it "fails on empty input" $ do
            parse `shouldFailOn` ""

        it "parses simple instructions" $ do
            parse "wait" `shouldParse` PDL.Instruction "wait" emptyArgs []

        it "parses instructions with arguments" $ do
            parse "pause [30ms]"
                `shouldParse` PDL.Instruction "pause" (PDL.Args [PDL.ArgDuration $ PDL.Milliseconds 30]) []

        it "parses instructions with nested expressions" $ do
            parse [r|type "this is some text"|]
                `shouldParse` PDL.Instruction "type" emptyArgs [PDL.Literal "this is some text"]

        it "parses instructions with arguments and nested expressions" $ do
            parse [r|typeWithDelay [30ms] "this is some text"|]
                `shouldParse` PDL.Instruction
                    "typeWithDelay"
                    (PDL.Args [PDL.ArgDuration $ PDL.Milliseconds 30])
                    [PDL.Literal "this is some text"]

    describe "parseSlide" $ do
        let parse = M.parse PDL.parseSlide ""
        it "parses empty slides" $ do
            parse "--- slide\n" `shouldParse` PDL.PSlide emptyArgs []
            parse "---slide\n" `shouldParse` PDL.PSlide emptyArgs []

        it "parses slide arguments" $ do
            parse "--- slide [1s]\n" `shouldParse` PDL.PSlide (PDL.Args [PDL.ArgDuration $ PDL.Seconds 1]) []

        it "parses slide contents" $ do
            parse
                """
                --- slide
                "hello"
                wait
                """
                `shouldParse` PDL.PSlide emptyArgs [PDL.Literal "hello", PDL.Instruction "wait" emptyArgs []]

    describe "parse presentation" $ do
        let parse = M.parse PDL.parsePresentation ""
        it "parses a single slide" $ do
            parse
                """
                --- slide
                "some text"
                """
                `shouldParse` PDL.Presentation
                    [PDL.PSlide emptyArgs [PDL.Literal "some text"]]

        it "parses multiple slides" $ do
            parse
                """
                --- slide
                "title"

                --- slide
                center "hi"
                wait
                type "some text"
                """
                `shouldParse` PDL.Presentation
                    [ PDL.PSlide emptyArgs [PDL.Literal "title"]
                    , PDL.PSlide
                        emptyArgs
                        [ PDL.Instruction "center" emptyArgs [PDL.Literal "hi"]
                        , PDL.Instruction "wait" emptyArgs []
                        , PDL.Instruction "type" emptyArgs [PDL.Literal "some text"]
                        ]
                    ]

emptyArgs :: PDL.Args
emptyArgs = PDL.Args []