module Compiler.RewriteSpec (spec) where

import Compiler.Helpers (shouldCompileTo)
import Compiler.Internal.Rewrite (rewrite, rewriteExpression)
import Data.Map qualified as M
import Parser.AST qualified as AST
import Runtime.Value qualified as Value
import Test.Hspec (Expectation, Spec, describe, it, pendingWith)

spec :: Spec
spec = do
    passThroughSpec
    rewriteLiteralLine
    rewriteMiddleSpec
    rewriteListSpec

    rewriteExpressions

passThroughSpec :: Spec
passThroughSpec = do
    describe "unmatch input" $ do
        it "does not modify literals" $
            AST.Literal "hello" `shouldRewriteExpressionTo` [AST.Literal "hello"]
        it "does not modify newlines" $
            AST.Newline `shouldRewriteExpressionTo` [AST.Newline]
        it "does not modify calls" $
            AST.Call "clear" M.empty [] `shouldRewriteExpressionTo` [AST.Call "clear" M.empty []]

rewriteLiteralLine :: Spec
rewriteLiteralLine = do
    describe "LiteralLine" $ do
        it "appends a newline to the text" $ do
            AST.LiteralLine "hello" `shouldRewriteExpressionTo` [AST.Literal "hello", AST.Newline]

rewriteMiddleSpec :: Spec
rewriteMiddleSpec = do
    describe "middle" $ do
        it "produces vcenter and center" $
            AST.Call
                "middle"
                M.empty
                [AST.Literal "middle text"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "vcenter"
                                                M.empty
                                                [ AST.Call
                                                    "center"
                                                    M.empty
                                                    [ AST.Literal "middle text"
                                                    ]
                                                ]
                                            ]

rewriteListSpec :: Spec
rewriteListSpec = do
    describe "list" $ do
        it "produces list items separated by newlines, waiting between items" $
            do
                AST.Call "list" M.empty [AST.Literal "first", AST.Literal "second", AST.Literal "third"]
                `shouldRewriteExpressionTo` [ AST.Literal "first"
                                            , AST.Newline
                                            , AST.Call "wait" M.empty []
                                            , AST.Literal "second"
                                            , AST.Newline
                                            , AST.Call "wait" M.empty []
                                            , AST.Literal "third"
                                            , AST.Newline
                                            ]

        it "prefixes a bullet point" $
            do
                AST.Call
                    "list"
                    (M.fromList [("bullet", Value.Literal "*")])
                    [ AST.Literal "first"
                    , AST.Literal "second"
                    ]
                `shouldRewriteExpressionTo` [ AST.Literal "* "
                                            , AST.Literal "first"
                                            , AST.Newline
                                            , AST.Call "wait" M.empty []
                                            , AST.Literal "* "
                                            , AST.Literal "second"
                                            , AST.Newline
                                            ]

rewriteExpressions :: Spec
rewriteExpressions = do
    describe "rewriteExpressions" $ do
        it "rewrites each expression" $ do
            rewrite [] `shouldCompileTo` []
            rewrite
                [ AST.Call "middle" M.empty [AST.Literal "hi"]
                , AST.LiteralLine "hello"
                ]
                `shouldCompileTo` [ AST.Call "vcenter" M.empty [AST.Call "center" M.empty [AST.Literal "hi"]]
                                  , AST.Literal "hello"
                                  , AST.Newline
                                  ]

        it "rewrites nested expressions" $ do
            rewrite
                [ AST.Call
                    "vcenter"
                    M.empty
                    [ AST.LiteralLine "hello"
                    , AST.LiteralLine "there"
                    ]
                ]
                `shouldCompileTo` [ AST.Call
                                        "vcenter"
                                        M.empty
                                        [ AST.Literal "hello"
                                        , AST.Newline
                                        , AST.Literal "there"
                                        , AST.Newline
                                        ]
                                  ]

            rewrite [AST.Call "middle" M.empty [AST.LiteralLine "hello"]]
                `shouldCompileTo` [ AST.Call
                                        "vcenter"
                                        M.empty
                                        [ AST.Call
                                            "center"
                                            M.empty
                                            [ AST.Literal "hello"
                                            , AST.Newline
                                            ]
                                        ]
                                  ]

        it "reapplies rewrite until everything is rewritten" $ pendingWith "no use case for this yet"

shouldRewriteExpressionTo :: AST.Expr -> [AST.Expr] -> Expectation
shouldRewriteExpressionTo e es = rewriteExpression e `shouldCompileTo` es
