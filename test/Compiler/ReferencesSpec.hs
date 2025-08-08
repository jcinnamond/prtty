module Compiler.ReferencesSpec (spec) where

import Compiler.Internal.References (extractDefinitions, extractExprs, partition, resolve, resolveReferences)
import Data.Map qualified as M
import Parser.AST qualified as AST
import Runtime.Value qualified as Runtime
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "extractDefinitions" $ do
        it "returns a map of definitions" $
            do
                extractDefinitions
                    [ AST.PDef "delay" (Runtime.Duration $ Runtime.Seconds 1)
                    , AST.PExpr $ AST.Literal "hi"
                    , AST.PDef "fg" (Runtime.RGB 0 0 0)
                    , AST.PExpr $ AST.Call "clear" M.empty []
                    ]
                `shouldBe` M.fromList
                    [ ("delay", Runtime.Duration $ Runtime.Seconds 1)
                    , ("fg", Runtime.RGB 0 0 0)
                    ]

    describe "extractExprs" $ do
        it "returns a list of expressions" $
            do
                extractExprs
                    [ AST.PDef "delay" (Runtime.Duration $ Runtime.Seconds 1)
                    , AST.PExpr $ AST.Literal "hi"
                    , AST.PDef "fg" (Runtime.RGB 0 0 0)
                    , AST.PExpr $ AST.Call "clear" M.empty []
                    ]
                `shouldBe` [ AST.Literal "hi"
                           , AST.Call "clear" M.empty []
                           ]

    describe "partition" $ do
        it "splits a presentation into definitions and expressions" $ do
            partition
                [ AST.Presentation
                    [ AST.PDef "delay" (Runtime.Duration $ Runtime.Seconds 1)
                    , AST.PExpr $ AST.Call "clear" M.empty []
                    ]
                , AST.Presentation
                    [ AST.PDef "padding" (Runtime.Number 10)
                    , AST.PExpr $ AST.Literal "some text"
                    ]
                ]
                `shouldBe` ( M.fromList
                                [ ("delay", Runtime.Duration $ Runtime.Seconds 1)
                                , ("padding", Runtime.Number 10)
                                ]
                           ,
                               [ AST.Call "clear" M.empty []
                               , AST.Literal "some text"
                               ]
                           )

    describe "resolve" $ do
        let definitions =
                M.fromList
                    [ ("delay", Runtime.Duration $ Runtime.Milliseconds 50)
                    , ("padding", Runtime.Number 10)
                    ]

        it "replaces references with their definitions" $
            do
                resolve definitions M.empty `shouldBe` Right M.empty

                resolve definitions (M.fromList [("delay", Runtime.Reference "delay")])
                    `shouldBe` Right (M.fromList [("delay", Runtime.Duration $ Runtime.Milliseconds 50)])

                resolve
                    definitions
                    ( M.fromList
                        [ ("x", Runtime.Reference "padding")
                        , ("y", Runtime.Reference "padding")
                        , ("anchor", Runtime.Literal "topLeft")
                        ]
                    )
                    `shouldBe` Right
                        ( M.fromList
                            [ ("x", Runtime.Number 10)
                            , ("y", Runtime.Number 10)
                            , ("anchor", Runtime.Literal "topLeft")
                            ]
                        )

        it "returns an error if the definition can't be found" $ do
            resolve definitions (M.fromList [("x", Runtime.Reference "xpos")])
                `shouldBe` Left "unresolved reference $xpos"

    describe "resolveReferences" $ do
        it "replaces references with their definition" $ do
            resolveReferences
                [ AST.Presentation
                    [ AST.PDef "highlight" (Runtime.RGB 255 127 220)
                    , AST.PExpr $ AST.Call "clear" M.empty []
                    ]
                , AST.Presentation
                    [ AST.PDef "margin" $ Runtime.Number 10
                    , AST.PExpr $
                        AST.Call
                            "center"
                            M.empty
                            [ AST.Call
                                "style"
                                (M.fromList [("fg", Runtime.Reference "highlight")])
                                [ AST.Literal "hi"
                                ]
                            ]
                    , AST.PExpr $ AST.Call "moveTo" (M.fromList [("x", Runtime.Reference "margin")]) []
                    ]
                ]
                `shouldBe` Right
                    [ AST.Call "clear" M.empty []
                    , AST.Call
                        "center"
                        M.empty
                        [ AST.Call "style" (M.fromList [("fg", Runtime.RGB 255 127 220)]) [AST.Literal "hi"]
                        ]
                    , AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10)]) []
                    ]

        it "returns an error if a reference can't be found" $
            do
                resolveReferences
                    [ AST.Presentation
                        [ AST.PExpr $
                            AST.Call
                                "margin"
                                ( M.fromList
                                    [ ("x", Runtime.Reference "padding")
                                    ]
                                )
                                []
                        ]
                    ]
                `shouldBe` Left "unresolved reference $padding"