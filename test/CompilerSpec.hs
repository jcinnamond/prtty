module CompilerSpec (spec) where

import Compiler.Compiler (compile)
import Compiler.Internal (compileExpression)
import Compiler.Internal.Types (evalCompiler)
import Data.Either (isLeft)
import Data.Map qualified as M
import Data.Vector qualified as V
import Options (Options (..))
import Parser.AST qualified as AST
import Runtime.Instructions qualified as Runtime
import Runtime.Value qualified as Runtime
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import VT qualified

spec :: Spec
spec = do
    compileLiteralSpec
    compileNewlineSpec
    compileSpec
    compilePreludeSpec
    compileJumpSpec

compileLiteralSpec :: Spec
compileLiteralSpec = do
    it "outputs the literal" $ do
        AST.Literal "some text" `shouldCompileTo` [Runtime.Output "some text"]

compileNewlineSpec :: Spec
compileNewlineSpec = do
    it "outputs the newline" $ do
        AST.Newline `shouldCompileTo` [Runtime.Newline]

compileSpec :: Spec
compileSpec = do
    it "compiles empty presentations" $
        compile options [] `shouldBe` Right V.empty

    it "compiles simple presentations" $
        compile
            options
            [ AST.Presentation [AST.PExpr $ AST.Call "clear" M.empty []]
            , AST.Presentation [AST.PExpr $ AST.Literal "hi"]
            ]
            `shouldBe` Right
                ( V.fromList
                    [ Runtime.StoreBackMarker
                    , Runtime.Output VT.clear
                    , Runtime.Home
                    , Runtime.Output "hi"
                    ]
                )

    it "compiles presentations with references" $
        compile
            options
            [ AST.Presentation [AST.PDef "margin" $ Runtime.Number 10]
            , AST.Presentation
                [ AST.PExpr $
                    AST.Call
                        "moveTo"
                        (M.fromList [("x", Runtime.Reference "margin")])
                        []
                ]
            ]
            `shouldBe` Right
                ( V.fromList
                    [ Runtime.MoveTo
                        Nothing
                        (Just $ Runtime.Number 10)
                        Runtime.Margin
                    ]
                )

    it "reduces expressions" $
        compile
            options
            [ AST.Presentation
                [ AST.PExpr $
                    AST.Call
                        "slide"
                        M.empty
                        [ AST.Call "middle" M.empty [AST.Literal "hello"]
                        ]
                , AST.PExpr $ AST.Call "slide" M.empty [AST.Literal "hi"]
                ]
            ]
            `shouldBe` Right
                ( V.fromList
                    [ Runtime.SetMarker "slide1"
                    , Runtime.StoreBackMarker
                    , Runtime.Output VT.clear
                    , Runtime.Home
                    , Runtime.VCenter 0
                    , Runtime.Center 5
                    , Runtime.Output "hello"
                    , Runtime.WaitForInput
                    , Runtime.SetMarker "slide2"
                    , Runtime.StoreBackMarker
                    , Runtime.Output VT.clear
                    , Runtime.Home
                    , Runtime.Output "hi"
                    , Runtime.WaitForInput
                    ]
                )

    it "returns an error if the presentation can't be compiled" $ do
        let result =
                compile
                    options
                    [AST.Presentation [AST.PExpr $ AST.Call "unknown" M.empty []]]
        isLeft result `shouldBe` True

compilePreludeSpec :: Spec
compilePreludeSpec = do
    describe "prelude" $ do
        it "is inserted at the start of the presentation" $
            do
                compile
                    options
                    [ AST.Presentation
                        [AST.PExpr $ AST.Literal "Hi"]
                    , AST.Presentation
                        [ AST.PExpr $
                            AST.Call
                                "prelude"
                                M.empty
                                [ AST.Call "margin" (M.fromList [("left", Runtime.Number 10)]) []
                                ]
                        , AST.PExpr $ AST.Literal "Bye"
                        ]
                    ]
                `shouldBe` Right
                    ( V.fromList
                        [ Runtime.SetLeftMargin $ Runtime.Number 10
                        , Runtime.Output "Hi"
                        , Runtime.Output "Bye"
                        ]
                    )

        it "combines multiple preludes" $
            do
                compile
                    options
                    [ AST.Presentation
                        [ AST.PExpr $ AST.Literal "Hi"
                        , AST.PExpr $ AST.Call "prelude" M.empty [AST.Literal "start of presentation"]
                        ]
                    , AST.Presentation
                        [ AST.PExpr $
                            AST.Call
                                "prelude"
                                M.empty
                                [ AST.Call "margin" (M.fromList [("left", Runtime.Number 10)]) []
                                ]
                        , AST.PExpr $ AST.Literal "Bye"
                        ]
                    ]
                `shouldBe` Right
                    ( V.fromList
                        [ Runtime.Output "start of presentation"
                        , Runtime.SetLeftMargin $ Runtime.Number 10
                        , Runtime.Output "Hi"
                        , Runtime.Output "Bye"
                        ]
                    )

compileJumpSpec :: Spec
compileJumpSpec = do
    describe "jump to custom marker" $ do
        it "starts at the marked instruction" $
            do
                compile
                    options{startAt = Just "marker1"}
                    [ AST.Presentation
                        [ AST.PExpr $ AST.Literal "something"
                        , AST.PExpr $ AST.Literal "something else"
                        , AST.PExpr $ AST.Call "waypoint" (M.fromList [("name", Runtime.Literal "marker1")]) []
                        , AST.PExpr $ AST.Literal "another thing"
                        ]
                    ]
                `shouldBe` Right
                    ( V.fromList
                        [ Runtime.JumpTo 3
                        , Runtime.Output "something"
                        , Runtime.Output "something else"
                        , Runtime.SetMarker "marker1"
                        , Runtime.Output "another thing"
                        ]
                    )

        it "doesn't jump until after the prelude" $
            do
                compile
                    options{startAt = Just "marker1"}
                    [ AST.Presentation
                        [ AST.PExpr $
                            AST.Call
                                "prelude"
                                M.empty
                                [ AST.Call "margin" (M.fromList [("left", Runtime.Number 10)]) []
                                ]
                        , AST.PExpr $ AST.Literal "something"
                        ]
                    , AST.Presentation
                        [ AST.PExpr $ AST.Literal "something else"
                        , AST.PExpr $ AST.Call "waypoint" (M.fromList [("name", Runtime.Literal "marker1")]) []
                        , AST.PExpr $ AST.Literal "another thing"
                        ]
                    ]
                `shouldBe` Right
                    ( V.fromList
                        [ Runtime.SetLeftMargin $ Runtime.Number 10
                        , Runtime.JumpTo 4
                        , Runtime.Output "something"
                        , Runtime.Output "something else"
                        , Runtime.SetMarker "marker1"
                        , Runtime.Output "another thing"
                        ]
                    )

options :: Options
options =
    Options
        { debugAST = False
        , debugIR = False
        , startAt = Nothing
        , inputs = []
        }

shouldCompileTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileTo expr is = evalCompiler (compileExpression expr) `shouldBe` Right (V.fromList is)
