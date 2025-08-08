{-# LANGUAGE QuasiQuotes #-}

module ParserSpec (spec) where

import Data.Map qualified as M
import Parser.AST qualified as AST
import Parser.Internal qualified as Parser
import Runtime.Value qualified as Runtime
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec qualified as MP
import Text.RawString.QQ (r)

spec :: Spec
spec = do
    parseLiteralsSpec
    parseIdentifier
    parseArgs
    parseCall
    parseDefinition
    parsePresentation

parseDefinition :: Spec
parseDefinition = do
    describe "simple definition" $ do
        let parse = MP.parse Parser.definition ""
        it "parses value definitions" $ do
            parse "\\x = 123" `shouldParse` AST.PDef "x" (Runtime.Number 123)

    describe "reference" $ do
        let parse = MP.parse Parser.reference ""
        it "parses" $ do
            parse "$x" `shouldParse` "x"

parseLiteralsSpec :: Spec
parseLiteralsSpec = do
    describe "literal" $ do
        let parse = MP.parse Parser.literal ""
        it "parses text" $ do
            parse "hello" `shouldParse` "hello"
            parse "hello there" `shouldParse` "hello there"
            parse "présentation" `shouldParse` "présentation"
            parse "⏳" `shouldParse` "⏳"
            parse [r|this is "quoted"|] `shouldParse` [r|this is "quoted"|]
        it "does not parse empty input" $ do
            parse `shouldFailOn` ""
            parse `shouldFailOn` " "
            parse `shouldFailOn` "\n"
        it "stops parsing on newlines" $ do
            parse "hello there\n" `shouldParse` "hello there"
        it "consumes combinators when unquoted" $ do
            parse "hello < there" `shouldParse` "hello < there"
            parse "hello << there" `shouldParse` "hello << there"

    describe "quoted literal" $ do
        let parse = MP.parse Parser.quotedLiteral ""
        it "parses text" $ do
            parse [r|"hello"|] `shouldParse` "hello"
        it "eats combinators" $ do
            parse [r|"hello < there"|] `shouldParse` "hello < there"
            parse [r|"hello << there"|] `shouldParse` "hello << there"

    describe "literal lines" $ do
        let parse = MP.parse Parser.literalLine ""
        it "extracts text" $ do
            parse "> some text" `shouldParse` "some text"
            parse ">some text" `shouldParse` "some text"

        it "only eats the first space" $ do
            parse ">     some text" `shouldParse` "    some text"

parseIdentifier :: Spec
parseIdentifier = do
    describe "identifiers" $ do
        let parse = MP.parse Parser.identifier ""
        it "parses words" $ do
            parse "center" `shouldParse` "center"
            parse "center " `shouldParse` "center"
            parse "présentation" `shouldParse` "présentation"
        it "does not parse symbols" $ do
            parse `shouldFailOn` "<<"
            parse `shouldFailOn` "=10"

parseArgs :: Spec
parseArgs = do
    describe "args" $ do
        let parse = MP.parse Parser.args ""
        it "parses integers" $ do
            parse "[x=10]" `shouldParse` M.fromList [("x", Runtime.Number 10)]
            parse "[ x = 10 ]" `shouldParse` M.fromList [("x", Runtime.Number 10)]
        it "parses rationals" $ do
            parse "[left=1/3]" `shouldParse` M.fromList [("left", Runtime.Rational 1 3)]
            parse "[left = 1/3]" `shouldParse` M.fromList [("left", Runtime.Rational 1 3)]
            parse "[left= 1 / 3]" `shouldParse` M.fromList [("left", Runtime.Rational 1 3)]
        it "parses percentages" $ do
            parse "[left=50%]" `shouldParse` M.fromList [("left", Runtime.Percentage 50)]
            parse "[left = 50%]" `shouldParse` M.fromList [("left", Runtime.Percentage 50)]
        it "parses durations" $ do
            parse "[delay=5s]" `shouldParse` M.fromList [("delay", Runtime.Duration (Runtime.Seconds 5))]
            parse "[delay=30ms]" `shouldParse` M.fromList [("delay", Runtime.Duration (Runtime.Milliseconds 30))]
        it "parses rgb hex codes" $ do
            parse "[color=#aa107f]" `shouldParse` M.fromList [("color", Runtime.RGB 170 16 127)]
            parse "[color=#AA107F]" `shouldParse` M.fromList [("color", Runtime.RGB 170 16 127)]
        it "parses literals" $ do
            parse "[align=center]" `shouldParse` M.fromList [("align", Runtime.Literal "center")]
            parse "[align= center ]" `shouldParse` M.fromList [("align", Runtime.Literal "center")]
        it "parses quoted literals" $ do
            parse [r|[cmd="kitten icat img.png"]|] `shouldParse` M.fromList [("cmd", Runtime.Literal "kitten icat img.png")]
        it "parses filepaths" $ do
            parse "[path=./some/path.png]" `shouldParse` M.fromList [("path", Runtime.Filepath "./some/path.png")]
            parse "[path=/absolute/path.png]" `shouldParse` M.fromList [("path", Runtime.Filepath "/absolute/path.png")]
            parse "[path=~/home/path.png]" `shouldParse` M.fromList [("path", Runtime.Filepath "~/home/path.png")]
            parse "[path = ~/home/path.png]" `shouldParse` M.fromList [("path", Runtime.Filepath "~/home/path.png")]
        it "parses toggles (keys without vaules)" $ do
            parse "[bold]" `shouldParse` M.fromList [("bold", Runtime.Toggle)]
        it "parses references" $ do
            parse "[delay=$d]" `shouldParse` M.fromList [("delay", Runtime.Reference "d")]

        it "parses multiple args" $ do
            parse "[x=1/3;y=2]"
                `shouldParse` M.fromList
                    [ ("x", Runtime.Rational 1 3)
                    , ("y", Runtime.Number 2)
                    ]
            parse "[ align = left ; bold ; offset = 10% ; delay = 10s ]"
                `shouldParse` M.fromList
                    [ ("align", Runtime.Literal "left")
                    , ("bold", Runtime.Toggle)
                    , ("offset", Runtime.Percentage 10)
                    , ("delay", Runtime.Duration (Runtime.Seconds 10))
                    ]

parseCall :: Spec
parseCall = do
    describe "calling functions" $ do
        let parse = MP.parse Parser.call ""
        it "parses functions without any arguments or blocks" $ do
            parse ".center" `shouldParse` AST.Call "center" M.empty []
        it "parses functions with arguments" $ do
            parse ".style [color=#0000ff]"
                `shouldParse` AST.Call
                    "style"
                    (M.fromList [("color", Runtime.RGB 0 0 255)])
                    []

        it "parses functions with <<" $ do
            parse ".center << .type < some text"
                `shouldParse` AST.Call
                    "center"
                    M.empty
                    [ AST.Call "type" M.empty []
                    , AST.Literal "some text"
                    ]

            parse ".center << .type << some text"
                `shouldParse` AST.Call
                    "center"
                    M.empty
                    [ AST.Call "type" M.empty [AST.Literal "some text"]
                    ]

        it "parses indented multiline functions" $ do
            parse
                """
                .type
                  some text 
                  some more text
                """
                `shouldParse` AST.Call "type" M.empty [AST.Literal "some text ", AST.Literal "some more text"]

            parse
                """
                .center
                  .type << some text
                  .type 
                    some more text
                """
                `shouldParse` AST.Call
                    "center"
                    M.empty
                    [ AST.Call "type" M.empty [AST.Literal "some text"]
                    , AST.Call "type" M.empty [AST.Literal "some more text"]
                    ]

        it "parses function calls with arguments and bodies" $ do
            parse ".style [color=#00ffff] << .type < some text"
                `shouldParse` AST.Call
                    "style"
                    (M.fromList [("color", Runtime.RGB 0 255 255)])
                    [ AST.Call "type" M.empty []
                    , AST.Literal "some text"
                    ]

parsePresentation :: Spec
parsePresentation = do
    describe "presentation" $ do
        let parse = MP.parse Parser.presentation ""
        it "parses a single literal" $ do
            parse "some text" `shouldParse` AST.Presentation [AST.PExpr $ AST.Literal "some text"]
        it "parses a literal line" $ do
            parse "> some text" `shouldParse` AST.Presentation [AST.PExpr $ AST.LiteralLine "some text"]
        it "parses a single call" $ do
            parse ".clear" `shouldParse` AST.Presentation [AST.PExpr $ AST.Call "clear" M.empty []]
        it "parses a single definition" $ do
            parse "\\delay = 1s" `shouldParse` AST.Presentation [AST.PDef "delay" (Runtime.Duration $ Runtime.Seconds 1)]

        it "ignores tailing lines" $ do
            parse "some text\n" `shouldParse` AST.Presentation [AST.PExpr $ AST.Literal "some text"]
            parse "some text\n\n" `shouldParse` AST.Presentation [AST.PExpr $ AST.Literal "some text"]

        it "parses expressions separated by <" $ do
            parse ".nl < .nl < some text"
                `shouldParse` AST.Presentation
                    [ AST.PExpr AST.Newline
                    , AST.PExpr AST.Newline
                    , AST.PExpr $ AST.Literal "some text"
                    ]

        it "parses quoted literals separated by <" $ do
            parse [r|"some text" < .nl|]
                `shouldParse` AST.Presentation
                    [ AST.PExpr $ AST.Literal "some text"
                    , AST.PExpr AST.Newline
                    ]

        it "parses expressions over multiple lines" $ do
            parse
                """
                .clear
                .vcenter
                .color [name=blue] << heading
                .nl
                some text
                .nl
                """
                `shouldParse` AST.Presentation
                    [ AST.PExpr $ AST.Call "clear" M.empty []
                    , AST.PExpr $ AST.Call "vcenter" M.empty []
                    , AST.PExpr $
                        AST.Call
                            "color"
                            (M.fromList [("name", Runtime.Literal "blue")])
                            [ AST.Literal "heading"
                            ]
                    , AST.PExpr AST.Newline
                    , AST.PExpr $ AST.Literal "some text"
                    , AST.PExpr AST.Newline
                    ]

        it "parses a sample presentation" $ do
            parse
                [r|
\delay = 1s

.slide
  .vcenter
    .center << .bold < .type << A presentation
    .pause [delay=$delay]
    .nl < .nl
    .center << .type << By a person

.slide
    .vcenter
    end of presentation
|]
                `shouldParse` AST.Presentation
                    [ AST.PDef "delay" (Runtime.Duration $ Runtime.Seconds 1)
                    , AST.PExpr $
                        AST.Call
                            "slide"
                            M.empty
                            [ AST.Call
                                "vcenter"
                                M.empty
                                [ AST.Call
                                    "center"
                                    M.empty
                                    [ AST.Call "bold" M.empty []
                                    , AST.Call "type" M.empty [AST.Literal "A presentation"]
                                    ]
                                , AST.Call "pause" (M.fromList [("delay", Runtime.Reference "delay")]) []
                                , AST.Newline
                                , AST.Newline
                                , AST.Call
                                    "center"
                                    M.empty
                                    [AST.Call "type" M.empty [AST.Literal "By a person"]]
                                ]
                            ]
                    , AST.PExpr $
                        AST.Call
                            "slide"
                            M.empty
                            [ AST.Call "vcenter" M.empty []
                            , AST.Literal "end of presentation"
                            ]
                    ]
