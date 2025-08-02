{-# LANGUAGE QuasiQuotes #-}

module ParserSpec (spec) where

import Parser.AST qualified as AST
import Parser.Internal qualified as Parser
import Runtime.Duration (Duration (..))
import Test.Hspec (Spec, describe, focus, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec qualified as M
import Text.RawString.QQ (r)

spec :: Spec
spec = focus $ do
    parseLiteralsSpec
    parseIdentifier
    parseArgs
    parseCall
    parsePresentation

parseLiteralsSpec :: Spec
parseLiteralsSpec = do
    describe "literal" $ do
        let parse = M.parse Parser.literal ""
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
        it "stops parsing on combinators" $ do
            parse "hello there >> bob" `shouldParse` "hello there "
            parse "hello there << bob" `shouldParse` "hello there "
            parse `shouldFailOn` "<<"

parseIdentifier :: Spec
parseIdentifier = do
    describe "identifiers" $ do
        let parse = M.parse Parser.identifier ""
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
        let parse = M.parse Parser.args ""
        it "parses integers" $ do
            parse "[x=10]" `shouldParse` [AST.Arg "x" (AST.ArgNumber 10)]
            parse "[ x = 10 ]" `shouldParse` [AST.Arg "x" (AST.ArgNumber 10)]
        it "parses rationals" $ do
            parse "[left=1/3]" `shouldParse` [AST.Arg "left" (AST.ArgRational 1 3)]
            parse "[left = 1/3]" `shouldParse` [AST.Arg "left" (AST.ArgRational 1 3)]
            parse "[left= 1 / 3]" `shouldParse` [AST.Arg "left" (AST.ArgRational 1 3)]
        it "parses percentages" $ do
            parse "[left=50%]" `shouldParse` [AST.Arg "left" (AST.ArgPercentage 50)]
            parse "[left = 50%]" `shouldParse` [AST.Arg "left" (AST.ArgPercentage 50)]
        it "parses durations" $ do
            parse "[delay=5s]" `shouldParse` [AST.Arg "delay" (AST.ArgDuration (Seconds 5))]
            parse "[delay=30ms]" `shouldParse` [AST.Arg "delay" (AST.ArgDuration (Milliseconds 30))]
        it "parses rgb hex codes" $ do
            parse "[color=#aa107f]" `shouldParse` [AST.Arg "color" (AST.ArgRGB 170 16 127)]
            parse "[color=#AA107F]" `shouldParse` [AST.Arg "color" (AST.ArgRGB 170 16 127)]
        it "parses literals" $ do
            parse "[align=center]" `shouldParse` [AST.Arg "align" (AST.ArgLiteral "center")]
            parse "[align= center ]" `shouldParse` [AST.Arg "align" (AST.ArgLiteral "center")]

        it "parses multiple args" $ do
            parse "[x=1/3;y=2]"
                `shouldParse` [ AST.Arg "x" (AST.ArgRational 1 3)
                              , AST.Arg "y" (AST.ArgNumber 2)
                              ]
            parse "[ align = left ; offset = 10% ; delay = 10s ]"
                `shouldParse` [ AST.Arg "align" (AST.ArgLiteral "left")
                              , AST.Arg "offset" (AST.ArgPercentage 10)
                              , AST.Arg "delay" (AST.ArgDuration (Seconds 10))
                              ]

parseCall :: Spec
parseCall = do
    describe "calling functions" $ do
        let parse = M.parse Parser.call ""
        it "parses functions without any arguments or blocks" $ do
            parse ".center" `shouldParse` AST.Call "center" [] []
        it "parses functions with arguments" $ do
            parse ".style [color=#0000ff]"
                `shouldParse` AST.Call
                    "style"
                    [AST.Arg "color" (AST.ArgRGB 0 0 255)]
                    []

        it "parses functions with <<" $ do
            parse ".center << .type < some text"
                `shouldParse` AST.Call
                    "center"
                    []
                    [ AST.Call "type" [] []
                    , AST.Literal "some text"
                    ]

            parse ".center << .type << some text"
                `shouldParse` AST.Call
                    "center"
                    []
                    [ AST.Call "type" [] [AST.Literal "some text"]
                    ]

        it "parses indented multiline functions" $ do
            parse
                """
                .type
                  some text 
                  some more text
                """
                `shouldParse` AST.Call "type" [] [AST.Literal "some text ", AST.Literal "some more text"]

            parse
                """
                .center
                  .type << some text
                  .type 
                    some more text
                """
                `shouldParse` AST.Call
                    "center"
                    []
                    [ AST.Call "type" [] [AST.Literal "some text"]
                    , AST.Call "type" [] [AST.Literal "some more text"]
                    ]

        it "parses function calls with arguments and bodies" $ do
            parse ".style [color=#00ffff] << .type < some text"
                `shouldParse` AST.Call
                    "style"
                    [AST.Arg "color" (AST.ArgRGB 0 255 255)]
                    [ AST.Call "type" [] []
                    , AST.Literal "some text"
                    ]

parsePresentation :: Spec
parsePresentation = do
    describe "presentation" $ do
        let parse = M.parse Parser.presentation ""
        it "parses a single literal" $ do
            parse "some text" `shouldParse` AST.Presentation [AST.Literal "some text"]
        it "parses a single call" $ do
            parse ".clear" `shouldParse` AST.Presentation [AST.Call "clear" [] []]

        it "ignores tailing lines" $ do
            parse "some text\n" `shouldParse` AST.Presentation [AST.Literal "some text"]
            parse "some text\n\n" `shouldParse` AST.Presentation [AST.Literal "some text"]

        it "parses expressions separated by <" $ do
            parse ".nl < .nl < some text"
                `shouldParse` AST.Presentation
                    [ AST.Newline
                    , AST.Newline
                    , AST.Literal "some text"
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
                    [ AST.Call "clear" [] []
                    , AST.Call "vcenter" [] []
                    , AST.Call
                        "color"
                        [AST.Arg "name" (AST.ArgLiteral "blue")]
                        [ AST.Literal "heading"
                        ]
                    , AST.Newline
                    , AST.Literal "some text"
                    , AST.Newline
                    ]

        it "parses a sample presentation" $ do
            parse
                [r|
.slide
  .vcenter
    .center << .bold < .type << A presentation
    .pause [delay=1s]
    .nl < .nl
    .center << .type << By a person

.slide
    .vcenter
    end of presentation
|]
                `shouldParse` AST.Presentation
                    [ AST.Call
                        "slide"
                        []
                        [ AST.Call
                            "vcenter"
                            []
                            [ AST.Call
                                "center"
                                []
                                [ AST.Call "bold" [] []
                                , AST.Call "type" [] [AST.Literal "A presentation"]
                                ]
                            , AST.Call "pause" [AST.Arg "delay" (AST.ArgDuration (Seconds 1))] []
                            , AST.Newline
                            , AST.Newline
                            , AST.Call
                                "center"
                                []
                                [AST.Call "type" [] [AST.Literal "By a person"]]
                            ]
                        ]
                    , AST.Call
                        "slide"
                        []
                        [ AST.Call "vcenter" [] []
                        , AST.Literal "end of presentation"
                        ]
                    ]
