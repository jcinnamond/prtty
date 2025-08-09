module Compiler.BuiltinSpec (spec) where

import Compiler.Internal (compileExpression)
import Compiler.Internal.Types (evalCompiler)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Instructions qualified as Runtime
import Runtime.Value qualified as Runtime
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import VT qualified

spec :: Spec
spec = do
    it "compiles 'clear'" $ do
        AST.Call "clear" M.empty []
            `shouldCompileExpressionTo` [ Runtime.StoreBackMarker
                                        , Runtime.Output VT.clear
                                        , Runtime.Home
                                        ]

    it "compiles 'wait'" $ do
        AST.Call "wait" M.empty [] `shouldCompileExpressionTo` [Runtime.WaitForInput]

    it "compiles slides" $ do
        AST.Call
            "slide"
            M.empty
            [ AST.Call
                "vcenter"
                M.empty
                [ AST.Literal "title"
                ]
            ]
            `shouldCompileExpressionTo` [ Runtime.StoreBackMarker
                                        , Runtime.Output VT.clear
                                        , Runtime.Home
                                        , Runtime.VCenter 0
                                        , Runtime.Output "title"
                                        , Runtime.WaitForInput
                                        ]

    describe "compile 'margin'" $ do
        it "compiles a left margin" $ do
            AST.Call "margin" (M.fromList [("left", Runtime.Percentage 10)]) [] `shouldCompileExpressionTo` [Runtime.SetLeftMargin $ Runtime.Percentage 10]
            AST.Call "margin" (M.fromList [("left", Runtime.Rational 1 10)]) [] `shouldCompileExpressionTo` [Runtime.SetLeftMargin $ Runtime.Rational 1 10]
            AST.Call "margin" (M.fromList [("left", Runtime.Number 5)]) [] `shouldCompileExpressionTo` [Runtime.SetLeftMargin $ Runtime.Number 5]

        it "compiles a top margin" $ do
            AST.Call "margin" (M.fromList [("top", Runtime.Percentage 10)]) [] `shouldCompileExpressionTo` [Runtime.SetTopMargin $ Runtime.Percentage 10]
            AST.Call "margin" (M.fromList [("top", Runtime.Rational 1 10)]) [] `shouldCompileExpressionTo` [Runtime.SetTopMargin $ Runtime.Rational 1 10]
            AST.Call "margin" (M.fromList [("top", Runtime.Number 5)]) [] `shouldCompileExpressionTo` [Runtime.SetTopMargin $ Runtime.Number 5]

        it "combines left and top margins" $ do
            AST.Call
                "margin"
                ( M.fromList
                    [ ("top", Runtime.Percentage 10)
                    , ("left", Runtime.Number 5)
                    ]
                )
                []
                `shouldCompileExpressionTo` [ Runtime.SetLeftMargin $ Runtime.Number 5
                                            , Runtime.SetTopMargin $ Runtime.Percentage 10
                                            ]

    it "compiles 'home'" $ do
        AST.Call "home" M.empty [] `shouldCompileExpressionTo` [Runtime.Home]

    describe "'moveTo'" $ do
        describe "without an anchor" $ do
            it "uses TopLeft as the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5)]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.Margin]
            it "compiles with just x coordinates" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10)]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo Nothing (Just $ Runtime.Number 10) Runtime.Margin]
            it "compiles with just y coordinates" $ do
                AST.Call "moveTo" (M.fromList [("y", Runtime.Number 5)]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo (Just $ Runtime.Number 5) Nothing Runtime.Margin]
        describe "with an explicit top left anchor" $ do
            it "compiles with the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5), ("anchor", Runtime.Literal "TopLeft")]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.TopLeft]
        describe "with an explicit bottom right anchor" $ do
            it "compiles with the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5), ("anchor", Runtime.Literal "BottomRight")]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.BottomRight]
        describe "with an explicit margin anchor" $ do
            it "compiles with the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5), ("anchor", Runtime.Literal "Margin")]) []
                    `shouldCompileExpressionTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.Margin]

    it "compiles 'center'" $ do
        AST.Call "center" M.empty [] `shouldCompileExpressionTo` [Runtime.Center 0]
        AST.Call "center" M.empty [AST.Literal "hi ", AST.Literal "there", AST.Newline]
            `shouldCompileExpressionTo` [ Runtime.Center 8
                                        , Runtime.Output "hi "
                                        , Runtime.Output "there"
                                        , Runtime.Newline
                                        ]
        AST.Call "center" M.empty [AST.Call "type" M.empty [AST.Literal "hello"]]
            `shouldCompileExpressionTo` [ Runtime.Center 5
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
        AST.Call "vcenter" M.empty [] `shouldCompileExpressionTo` [Runtime.VCenter 0]
        AST.Call "vcenter" M.empty [AST.Literal "hi"] `shouldCompileExpressionTo` [Runtime.VCenter 0, Runtime.Output "hi"]
        AST.Call
            "vcenter"
            M.empty
            [ AST.Literal "hi"
            , AST.Newline
            , AST.Call "center" M.empty [AST.Literal "there", AST.Newline]
            ]
            `shouldCompileExpressionTo` [ Runtime.VCenter 2
                                        , Runtime.Output "hi"
                                        , Runtime.Newline
                                        , Runtime.Center 5
                                        , Runtime.Output "there"
                                        , Runtime.Newline
                                        ]
        AST.Call
            "vcenter"
            M.empty
            [ AST.Call "vspace" (M.fromList [("lines", Runtime.Number 5)]) []
            , AST.Literal "hello"
            , AST.Newline
            ]
            `shouldCompileExpressionTo` [ Runtime.VCenter 6
                                        , Runtime.VSpace 5
                                        , Runtime.Output "hello"
                                        , Runtime.Newline
                                        ]

    it "complies 'vspace'" $ do
        AST.Call "vspace" M.empty [] `shouldCompileExpressionTo` [Runtime.VSpace 1]
        AST.Call "vspace" (M.fromList [("lines", Runtime.Number 5)]) [] `shouldCompileExpressionTo` [Runtime.VSpace 5]

    describe "'type'" $ do
        it "compiles with a default pause time" $ do
            AST.Call "type" M.empty [AST.Literal "hi there"]
                `shouldCompileExpressionTo` [ Runtime.Output "h"
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

        it "types multiple literals" $ do
            AST.Call "type" M.empty [AST.Literal "hi", AST.Literal "there"]
                `shouldCompileExpressionTo` [ Runtime.Output "h"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "i"
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

        it "passes through non-literal expressions" $ do
            AST.Call "type" M.empty [AST.Literal "hi", AST.Call "wait" M.empty [], AST.Literal "there"]
                `shouldCompileExpressionTo` [ Runtime.Output "h"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "i"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.WaitForInput
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

        it "pushes the type down to embedded literals" $ do
            AST.Call
                "type"
                M.empty
                [ AST.Literal "he"
                , AST.Call
                    "style"
                    (M.fromList [("fg", Runtime.RGB 0 0 0)])
                    [ AST.Literal "llo"
                    ]
                , AST.Literal "there"
                ]
                `shouldCompileExpressionTo` [ Runtime.Output "h"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "e"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 0 0 0})
                                            , Runtime.Output "l"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "l"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "o"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.RestoreStyle
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
                `shouldCompileExpressionTo` [ Runtime.Output "h"
                                            , Runtime.Pause (Runtime.Seconds 1)
                                            , Runtime.Output "i"
                                            , Runtime.Pause (Runtime.Seconds 1)
                                            ]

        it "types multiple literal lines" $ do
            AST.Call "type" M.empty [AST.LiteralLine "hi", AST.LiteralLine "there"]
                `shouldCompileExpressionTo` [ Runtime.Output "h"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Output "i"
                                            , Runtime.Pause (Runtime.Milliseconds 50)
                                            , Runtime.Newline
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
                                            , Runtime.Newline
                                            ]

    describe "'style'" $ do
        describe "without a block" $ do
            it "sets bold" $
                AST.Call "style" (M.fromList [("bold", Runtime.Toggle)]) []
                    `shouldCompileExpressionTo` [Runtime.SetStyle (nostyle{Runtime.bold = Just Runtime.Toggle})]
            it "sets fg color" $
                AST.Call "style" (M.fromList [("fg", Runtime.RGB 77 77 77)]) []
                    `shouldCompileExpressionTo` [Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 77 77 77})]
            it "sets bg color" $
                AST.Call "style" (M.fromList [("bg", Runtime.RGB 77 77 77)]) []
                    `shouldCompileExpressionTo` [Runtime.SetStyle (nostyle{Runtime.bgColor = Just $ Runtime.RGB 77 77 77})]
        describe "with a block" $ do
            it "temporarily sets the styles" $ do
                AST.Call
                    "style"
                    (M.fromList [("bold", Runtime.Toggle)])
                    [ AST.Literal "some text"
                    ]
                    `shouldCompileExpressionTo` [ Runtime.SaveStyle
                                                , Runtime.SetStyle (nostyle{Runtime.bold = Just Runtime.Toggle})
                                                , Runtime.Output "some text"
                                                , Runtime.RestoreStyle
                                                ]

    describe "'img'" $ do
        it "generates an instruction to show the image" $
            AST.Call "image" (M.fromList [("path", Runtime.Filepath "path.png")]) []
                `shouldCompileExpressionTo` [Runtime.Exec $ "kitten icat --align center " <> "path.png"]

    describe "'exec'" $ do
        it "generates an instruction to execute a command" $
            AST.Call "exec" (M.fromList [("cmd", Runtime.Literal "cmd and args")]) []
                `shouldCompileExpressionTo` [Runtime.Exec "cmd and args"]
        it "resets the terminal for interactive programs" $
            AST.Call
                "exec"
                ( M.fromList
                    [ ("cmd", Runtime.Literal "cmd")
                    , ("interactive", Runtime.Toggle)
                    ]
                )
                []
                `shouldCompileExpressionTo` [ Runtime.Exec "cmd"
                                            , Runtime.Reset
                                            ]

    describe "'quote'" $ do
        it "compiles quotes without an author" $ do
            AST.Call "quote" M.empty [AST.Literal "a quote"]
                `shouldCompileExpressionTo` [ Runtime.Center 9
                                            , Runtime.Output "“"
                                            , Runtime.Output "a quote"
                                            , Runtime.Output "”"
                                            ]
        it "compiles quotes with an author" $
            let q = "A quote that is longer than the author name"
                qlen = T.length q + 2
                citation = "Betty Author"
                plen = qlen - T.length citation - 2
                padding = T.replicate plen " "
             in AST.Call
                    "quote"
                    (M.fromList [("citation", Runtime.Literal citation)])
                    [AST.Literal q]
                    `shouldCompileExpressionTo` [ Runtime.Center qlen
                                                , Runtime.Output "“"
                                                , Runtime.Output q
                                                , Runtime.Output "”"
                                                , Runtime.Newline
                                                , Runtime.Center qlen
                                                , Runtime.Output $ padding <> "- " <> citation
                                                ]

        it "styles the quotation marks" $
            AST.Call "quote" (M.fromList [("altColor", Runtime.RGB 127 127 127)]) [AST.Literal "a quote"]
                `shouldCompileExpressionTo` [ Runtime.Center 9
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 127 127 127})
                                            , Runtime.Output "“"
                                            , Runtime.RestoreStyle
                                            , Runtime.Output "a quote"
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 127 127 127})
                                            , Runtime.Output "”"
                                            , Runtime.RestoreStyle
                                            ]

        it "styles the quotation marks and citation" $
            AST.Call
                "quote"
                ( M.fromList
                    [ ("citation", Runtime.Literal "person")
                    , ("altColor", Runtime.RGB 127 127 127)
                    ]
                )
                [AST.Literal "a longish quote"]
                `shouldCompileExpressionTo` [ Runtime.Center 17
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 127 127 127})
                                            , Runtime.Output "“"
                                            , Runtime.RestoreStyle
                                            , Runtime.Output "a longish quote"
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 127 127 127})
                                            , Runtime.Output "”"
                                            , Runtime.RestoreStyle
                                            , Runtime.Newline
                                            , Runtime.Center 17
                                            , Runtime.SaveStyle
                                            , Runtime.SetStyle (nostyle{Runtime.fgColor = Just $ Runtime.RGB 127 127 127})
                                            , Runtime.Output "         - person"
                                            , Runtime.RestoreStyle
                                            ]

    describe "'backspace'" $ do
        it "deletes characters backwards, pausing between" $ do
            AST.Call "backspace" (M.fromList [("count", Runtime.Number 3)]) []
                `shouldCompileExpressionTo` [ Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            ]

        it "deletes the length of a given literal" $ do
            AST.Call "backspace" M.empty [AST.Literal "hi"]
                `shouldCompileExpressionTo` [ Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            ]

        it "allows the pause time to be overridden" $ do
            AST.Call
                "backspace"
                ( M.fromList
                    [ ("delay", Runtime.Duration (Runtime.Milliseconds 2))
                    , ("count", Runtime.Number 2)
                    ]
                )
                []
                `shouldCompileExpressionTo` [ Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            ]

            AST.Call "backspace" (M.fromList [("delay", Runtime.Duration (Runtime.Milliseconds 2))]) [AST.Literal "hi"]
                `shouldCompileExpressionTo` [ Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            ]

    describe "'alternate'" $ do
        it "types, waits, and then deletes literals" $
            AST.Call
                "alternate"
                M.empty
                [ AST.Literal "first"
                , AST.Literal "second"
                ]
                `shouldCompileExpressionTo` [ Runtime.Output "f"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "i"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "r"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "s"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "t"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.WaitForInput
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "s"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "e"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "c"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "o"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "n"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            , Runtime.Output "d"
                                            , Runtime.Pause $ Runtime.Milliseconds 50
                                            ]

        it "allows the delay to be overridden" $
            AST.Call
                "alternate"
                (M.fromList [("delay", Runtime.Duration $ Runtime.Milliseconds 2)])
                [AST.Literal "first", AST.Literal "second"]
                `shouldCompileExpressionTo` [ Runtime.Output "f"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "i"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "r"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "s"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "t"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.WaitForInput
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "s"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "e"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "c"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "o"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "n"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            , Runtime.Output "d"
                                            , Runtime.Pause $ Runtime.Milliseconds 2
                                            ]

nostyle :: Runtime.Style
nostyle = Runtime.emptyStyle

shouldCompileExpressionTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileExpressionTo expr is = evalCompiler (compileExpression expr) `shouldBe` Right (V.fromList is)
