module CompilerSpec (spec) where

import Compiler.Internal (compileExpression)
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Duration qualified as Duration
import Runtime.Instructions qualified as Runtime
import Test.Hspec (Expectation, Spec, describe, focus, it, shouldBe)
import VT qualified

spec :: Spec
spec = focus $ do
    compileLiteralSpec
    compileNewlineSpec
    compileBuiltinSpec

compileLiteralSpec :: Spec
compileLiteralSpec = do
    it "outputs the literal" $ do
        AST.Literal "some text" `shouldCompileTo` [Runtime.Output "some text"]

compileNewlineSpec :: Spec
compileNewlineSpec = do
    it "outputs the newline" $ do
        AST.Newline `shouldCompileTo` [Runtime.Newline]

compileBuiltinSpec :: Spec
compileBuiltinSpec = do
    it "compiles 'clear'" $ do
        AST.Call "clear" [] []
            `shouldCompileTo` [ Runtime.StoreBackMarker
                              , Runtime.Output VT.clear
                              , Runtime.Home
                              ]

    it "compiles 'wait'" $ do
        AST.Call "wait" [] [] `shouldCompileTo` [Runtime.WaitForInput]

    describe "compile 'margin'" $ do
        it "compiles a left margin" $ do
            AST.Call "margin" [AST.Arg "left" $ AST.ArgPercentage 10] [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Percent 10]
            AST.Call "margin" [AST.Arg "left" $ AST.ArgRational 1 10] [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Rational 1 10]
            AST.Call "margin" [AST.Arg "left" $ AST.ArgNumber 5] [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Number 5]

        it "compiles a top margin" $ do
            AST.Call "margin" [AST.Arg "top" $ AST.ArgPercentage 10] [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Percent 10]
            AST.Call "margin" [AST.Arg "top" $ AST.ArgRational 1 10] [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Rational 1 10]
            AST.Call "margin" [AST.Arg "top" $ AST.ArgNumber 5] [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Number 5]

        it "combines left and top margins" $ do
            AST.Call
                "margin"
                [ AST.Arg "top" $ AST.ArgPercentage 10
                , AST.Arg "left" $ AST.ArgNumber 5
                ]
                []
                `shouldCompileTo` [ Runtime.SetLeftMargin $ Runtime.Number 5
                                  , Runtime.SetTopMargin $ Runtime.Percent 10
                                  ]

    it "compiles 'home'" $ do
        AST.Call "home" [] [] `shouldCompileTo` [Runtime.Home]

    it "compiles 'center'" $ do
        AST.Call "center" [] [] `shouldCompileTo` [Runtime.Center 0]
        AST.Call "center" [] [AST.Literal "hi ", AST.Literal "there", AST.Newline]
            `shouldCompileTo` [ Runtime.Center 8
                              , Runtime.Output "hi "
                              , Runtime.Output "there"
                              , Runtime.Newline
                              ]
        AST.Call "center" [] [AST.Call "type" [] [AST.Literal "hello"]]
            `shouldCompileTo` [ Runtime.Center 5
                              , Runtime.Output "h"
                              , Runtime.Pause (Duration.Milliseconds 50)
                              , Runtime.Output "e"
                              , Runtime.Pause (Duration.Milliseconds 50)
                              , Runtime.Output "l"
                              , Runtime.Pause (Duration.Milliseconds 50)
                              , Runtime.Output "l"
                              , Runtime.Pause (Duration.Milliseconds 50)
                              , Runtime.Output "o"
                              , Runtime.Pause (Duration.Milliseconds 50)
                              ]

    it "compiles 'vcenter'" $ do
        AST.Call "vcenter" [] [] `shouldCompileTo` [Runtime.VCenter 0]
        AST.Call "vcenter" [] [AST.Literal "hi"] `shouldCompileTo` [Runtime.VCenter 0, Runtime.Output "hi"]
        AST.Call
            "vcenter"
            []
            [ AST.Literal "hi"
            , AST.Newline
            , AST.Call "center" [] [AST.Literal "there", AST.Newline]
            ]
            `shouldCompileTo` [ Runtime.VCenter 2
                              , Runtime.Output "hi"
                              , Runtime.Newline
                              , Runtime.Center 5
                              , Runtime.Output "there"
                              , Runtime.Newline
                              ]

    describe "'type'" $ do
        it "compiles with a default pause time" $ do
            AST.Call "type" [] [AST.Literal "hi there"]
                `shouldCompileTo` [ Runtime.Output "h"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "i"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output " "
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "t"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "h"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "e"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "r"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  , Runtime.Output "e"
                                  , Runtime.Pause (Duration.Milliseconds 50)
                                  ]
        it "compiles with a custom delay" $ do
            AST.Call "type" [AST.Arg "delay" (AST.ArgDuration (Duration.Seconds 1))] [AST.Literal "hi"]
                `shouldCompileTo` [ Runtime.Output "h"
                                  , Runtime.Pause (Duration.Seconds 1)
                                  , Runtime.Output "i"
                                  , Runtime.Pause (Duration.Seconds 1)
                                  ]

shouldCompileTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileTo expr is = compileExpression expr `shouldBe` Right (V.fromList is)
