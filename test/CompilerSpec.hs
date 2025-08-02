module CompilerSpec (spec) where

import Compiler.Internal (compileExpression)
import Data.Map qualified as M
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Instructions qualified as Runtime
import Runtime.Value qualified as Runtime
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
        AST.Call "clear" M.empty []
            `shouldCompileTo` [ Runtime.StoreBackMarker
                              , Runtime.Output VT.clear
                              , Runtime.Home
                              ]

    it "compiles 'wait'" $ do
        AST.Call "wait" M.empty [] `shouldCompileTo` [Runtime.WaitForInput]

    describe "compile 'margin'" $ do
        it "compiles a left margin" $ do
            AST.Call "margin" (M.fromList [("left", Runtime.Percentage 10)]) [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Percentage 10]
            AST.Call "margin" (M.fromList [("left", Runtime.Rational 1 10)]) [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Rational 1 10]
            AST.Call "margin" (M.fromList [("left", Runtime.Number 5)]) [] `shouldCompileTo` [Runtime.SetLeftMargin $ Runtime.Number 5]

        it "compiles a top margin" $ do
            AST.Call "margin" (M.fromList [("top", Runtime.Percentage 10)]) [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Percentage 10]
            AST.Call "margin" (M.fromList [("top", Runtime.Rational 1 10)]) [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Rational 1 10]
            AST.Call "margin" (M.fromList [("top", Runtime.Number 5)]) [] `shouldCompileTo` [Runtime.SetTopMargin $ Runtime.Number 5]

        it "combines left and top margins" $ do
            AST.Call
                "margin"
                ( M.fromList
                    [ ("top", Runtime.Percentage 10)
                    , ("left", Runtime.Number 5)
                    ]
                )
                []
                `shouldCompileTo` [ Runtime.SetLeftMargin $ Runtime.Number 5
                                  , Runtime.SetTopMargin $ Runtime.Percentage 10
                                  ]

    it "compiles 'home'" $ do
        AST.Call "home" M.empty [] `shouldCompileTo` [Runtime.Home]

    it "compiles 'center'" $ do
        AST.Call "center" M.empty [] `shouldCompileTo` [Runtime.Center 0]
        AST.Call "center" M.empty [AST.Literal "hi ", AST.Literal "there", AST.Newline]
            `shouldCompileTo` [ Runtime.Center 8
                              , Runtime.Output "hi "
                              , Runtime.Output "there"
                              , Runtime.Newline
                              ]
        AST.Call "center" M.empty [AST.Call "type" M.empty [AST.Literal "hello"]]
            `shouldCompileTo` [ Runtime.Center 5
                              , Runtime.Output "h"
                              , Runtime.Pause (Runtime.Milliseconds 50)
                              , Runtime.Output "e"
                              , Runtime.Pause (Runtime.Milliseconds 50)
                              , Runtime.Output "l"
                              , Runtime.Pause (Runtime.Milliseconds 50)
                              , Runtime.Output "l"
                              , Runtime.Pause (Runtime.Milliseconds 50)
                              , Runtime.Output "o"
                              , Runtime.Pause (Runtime.Milliseconds 50)
                              ]

    it "compiles 'vcenter'" $ do
        AST.Call "vcenter" M.empty [] `shouldCompileTo` [Runtime.VCenter 0]
        AST.Call "vcenter" M.empty [AST.Literal "hi"] `shouldCompileTo` [Runtime.VCenter 0, Runtime.Output "hi"]
        AST.Call
            "vcenter"
            M.empty
            [ AST.Literal "hi"
            , AST.Newline
            , AST.Call "center" M.empty [AST.Literal "there", AST.Newline]
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
            AST.Call "type" M.empty [AST.Literal "hi there"]
                `shouldCompileTo` [ Runtime.Output "h"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "i"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output " "
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "t"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "h"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "e"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "r"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  , Runtime.Output "e"
                                  , Runtime.Pause (Runtime.Milliseconds 50)
                                  ]
        it "compiles with a custom delay" $ do
            AST.Call "type" (M.fromList [("delay", Runtime.Duration (Runtime.Seconds 1))]) [AST.Literal "hi"]
                `shouldCompileTo` [ Runtime.Output "h"
                                  , Runtime.Pause (Runtime.Seconds 1)
                                  , Runtime.Output "i"
                                  , Runtime.Pause (Runtime.Seconds 1)
                                  ]

    describe "'style'" $ do
        describe "without a block" $ do
            it "sets bold" $
                AST.Call "style" (M.fromList [("bold", Runtime.Toggle)]) []
                    `shouldCompileTo` [Runtime.SetStyle (nostyle{Runtime.bold = Just Runtime.Toggle})]
            it "sets fg color" $
                AST.Call "style" (M.fromList [("fg", Runtime.RGB 77 77 77)]) []
                    `shouldCompileTo` [Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 77 77 77})]
            it "sets bg color" $
                AST.Call "style" (M.fromList [("bg", Runtime.RGB 77 77 77)]) []
                    `shouldCompileTo` [Runtime.SetStyle (nostyle{Runtime.bgColor = Just $ Runtime.RGB 77 77 77})]
        describe "with a block" $ do
            it "temporarily sets the styles" $ do
                AST.Call
                    "style"
                    (M.fromList [("bold", Runtime.Toggle)])
                    [ AST.Literal "some text"
                    ]
                    `shouldCompileTo` [ Runtime.SaveStyle
                                      , Runtime.SetStyle (nostyle{Runtime.bold = Just Runtime.Toggle})
                                      , Runtime.Output "some text"
                                      , Runtime.RestoreStyle
                                      ]

nostyle :: Runtime.Style
nostyle = Runtime.emptyStyle

shouldCompileTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileTo expr is = compileExpression expr `shouldBe` Right (V.fromList is)
