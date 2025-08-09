module CompilerSpec (spec) where

import Compiler.Compiler (compile)
import Compiler.Internal (compileExpression)
import Data.Either (isLeft)
import Data.Map qualified as M
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Instructions qualified as Runtime
import Runtime.Value qualified as Runtime
import Test.Hspec (Expectation, Spec, it, shouldBe)
import VT qualified

spec :: Spec
spec = do
    compileLiteralSpec
    compileNewlineSpec
    compileSpec

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
        compile [] `shouldBe` Right V.empty

    it "compiles simple presentations" $
        compile
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
            [ AST.Presentation
                [ AST.PExpr $
                    AST.Call
                        "slide"
                        M.empty
                        [ AST.Call "middle" M.empty [AST.Literal "hello"]
                        ]
                ]
            ]
            `shouldBe` Right
                ( V.fromList
                    [ Runtime.StoreBackMarker
                    , Runtime.Output VT.clear
                    , Runtime.Home
                    , Runtime.VCenter 0
                    , Runtime.Center 5
                    , Runtime.Output "hello"
                    , Runtime.WaitForInput
                    ]
                )

    it "returns an error if the presentation can't be compiled" $ do
        let result = compile [AST.Presentation [AST.PExpr $ AST.Call "unknown" M.empty []]]
        isLeft result `shouldBe` True

shouldCompileTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileTo expr is = compileExpression expr `shouldBe` Right (V.fromList is)
