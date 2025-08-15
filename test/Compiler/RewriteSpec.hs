module Compiler.RewriteSpec (spec) where

import Compiler.Internal.Rewrite (rewrite, rewriteExpression)
import Data.Map qualified as M
import Parser.AST qualified as AST
import Runtime.Value qualified as Value
import Test.Hspec (Expectation, Spec, describe, it, pendingWith, shouldBe)

spec :: Spec
spec = do
    passThroughSpec
    rewriteLiteralLine
    rewriteMiddleSpec
    rewriteListSpec
    rewriteQuoteSpec
    rewriteAlternateSpec

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

rewriteQuoteSpec :: Spec
rewriteQuoteSpec = do
    describe "quote" $ do
        it "centers the text, wrapped with quotation marks" $
            AST.Call "quote" M.empty [AST.Literal "a quote"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "“"
                                                , AST.Literal "a quote"
                                                , AST.Literal "”"
                                                ]
                                            , AST.Newline
                                            ]

        it "centers two lines" $
            AST.Call "quote" M.empty [AST.Literal "first line", AST.Literal "second line"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "“"
                                                , AST.Literal "first line"
                                                ]
                                            , AST.Newline
                                            , AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "second line"
                                                , AST.Literal "”"
                                                ]
                                            , AST.Newline
                                            ]

        it "centers multiple lines" $
            AST.Call
                "quote"
                M.empty
                [ AST.Literal "first line"
                , AST.Literal "second line"
                , AST.Literal "third line"
                , AST.Literal "fourth line"
                ]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "“"
                                                , AST.Literal "first line"
                                                ]
                                            , AST.Newline
                                            , AST.Call "center" M.empty [AST.Literal "second line"]
                                            , AST.Newline
                                            , AST.Call "center" M.empty [AST.Literal "third line"]
                                            , AST.Newline
                                            , AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "fourth line"
                                                , AST.Literal "”"
                                                ]
                                            , AST.Newline
                                            ]

        it "adds a citation" $
            AST.Call "quote" (M.fromList [("citation", Value.Literal "a person")]) [AST.Literal "a long-ish quote"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "“"
                                                , AST.Literal "a long-ish quote"
                                                , AST.Literal "”"
                                                ]
                                            , AST.Newline
                                            , AST.Call "center" M.empty [AST.Literal "       - a person"]
                                            , AST.Newline
                                            ]

        it "aligns the quote with the longest line" $
            AST.Call
                "quote"
                (M.fromList [("citation", Value.Literal "a person")])
                [ AST.Literal "a very long line to open the quote"
                , AST.Literal "with a short line"
                ]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "“"
                                                , AST.Literal "a very long line to open the quote"
                                                ]
                                            , AST.Newline
                                            , AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Literal "with a short line"
                                                , AST.Literal "”"
                                                ]
                                            , AST.Newline
                                            , AST.Call "center" M.empty [AST.Literal "                         - a person"]
                                            , AST.Newline
                                            ]

        it "styles quotation marks" $
            AST.Call "quote" (M.fromList [("altColor", Value.RGB 7 7 7)]) [AST.Literal "a quote"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Call "style" (M.fromList [("fg", Value.RGB 7 7 7)]) [AST.Literal "“"]
                                                , AST.Literal "a quote"
                                                , AST.Call "style" (M.fromList [("fg", Value.RGB 7 7 7)]) [AST.Literal "”"]
                                                ]
                                            , AST.Newline
                                            ]
        it "styles citations" $
            AST.Call
                "quote"
                ( M.fromList
                    [ ("citation", Value.Literal "bob")
                    , ("altColor", Value.RGB 7 7 7)
                    ]
                )
                [AST.Literal "a quote"]
                `shouldRewriteExpressionTo` [ AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Call "style" (M.fromList [("fg", Value.RGB 7 7 7)]) [AST.Literal "“"]
                                                , AST.Literal "a quote"
                                                , AST.Call "style" (M.fromList [("fg", Value.RGB 7 7 7)]) [AST.Literal "”"]
                                                ]
                                            , AST.Newline
                                            , AST.Call
                                                "center"
                                                M.empty
                                                [ AST.Call "style" (M.fromList [("fg", Value.RGB 7 7 7)]) [AST.Literal "   - bob"]
                                                ]
                                            , AST.Newline
                                            ]

rewriteAlternateSpec :: Spec
rewriteAlternateSpec = do
    describe "alternate" $ do
        it "types, waits, and then deletes literals" $
            AST.Call "alternate" M.empty [AST.Literal "first", AST.Literal "second"]
                `shouldRewriteExpressionTo` [ AST.Call "type" M.empty [AST.Literal "first"]
                                            , AST.Call "wait" M.empty []
                                            , AST.Call "backspace" M.empty [AST.Literal "first"]
                                            , AST.Call "type" M.empty [AST.Literal "second"]
                                            ]

        it "allows the delay to be overridden" $
            AST.Call "alternate" (M.fromList [("delay", Value.Duration $ Value.Milliseconds 2)]) [AST.Literal "first", AST.Literal "second"]
                `shouldRewriteExpressionTo` [ AST.Call "type" (M.fromList [("delay", Value.Duration $ Value.Milliseconds 2)]) [AST.Literal "first"]
                                            , AST.Call "wait" M.empty []
                                            , AST.Call "backspace" (M.fromList [("delay", Value.Duration $ Value.Milliseconds 2)]) [AST.Literal "first"]
                                            , AST.Call "type" (M.fromList [("delay", Value.Duration $ Value.Milliseconds 2)]) [AST.Literal "second"]
                                            ]
rewriteExpressions :: Spec
rewriteExpressions = do
    describe "rewriteExpressions" $ do
        it "rewrites each expression" $ do
            rewrite [] `shouldBe` Right []
            rewrite
                [ AST.Call "middle" M.empty [AST.Literal "hi"]
                , AST.LiteralLine "hello"
                ]
                `shouldBe` Right
                    [ AST.Call "vcenter" M.empty [AST.Call "center" M.empty [AST.Literal "hi"]]
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
                `shouldBe` Right
                    [ AST.Call
                        "vcenter"
                        M.empty
                        [ AST.Literal "hello"
                        , AST.Newline
                        , AST.Literal "there"
                        , AST.Newline
                        ]
                    ]

            rewrite [AST.Call "middle" M.empty [AST.LiteralLine "hello"]]
                `shouldBe` Right
                    [ AST.Call
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
shouldRewriteExpressionTo e es = rewriteExpression e `shouldBe` Right es
