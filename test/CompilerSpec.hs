module CompilerSpec (spec) where

import Compiler.Internal (compileExpression)
import Data.Map qualified as M
import Data.Text qualified as T
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
            `shouldCompileTo` [ Runtime.StoreBackMarker
                              , Runtime.Output VT.clear
                              , Runtime.Home
                              , Runtime.VCenter 0
                              , Runtime.Output "title"
                              , Runtime.WaitForInput
                              ]

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

    describe "'moveTo'" $ do
        describe "without an anchor" $ do
            it "uses TopLeft as the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5)]) []
                    `shouldCompileTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.TopLeft]
            it "compiles with just x coordinates" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10)]) []
                    `shouldCompileTo` [Runtime.MoveTo Nothing (Just $ Runtime.Number 10) Runtime.TopLeft]
            it "compiles with just y coordinates" $ do
                AST.Call "moveTo" (M.fromList [("y", Runtime.Number 5)]) []
                    `shouldCompileTo` [Runtime.MoveTo (Just $ Runtime.Number 5) Nothing Runtime.TopLeft]
        describe "with an explicit top left anchor" $ do
            it "compiles with the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5), ("TopLeft", Runtime.Toggle)]) []
                    `shouldCompileTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.TopLeft]
        describe "with an explicit bottom right anchor" $ do
            it "compiles with the anchor" $ do
                AST.Call "moveTo" (M.fromList [("x", Runtime.Number 10), ("y", Runtime.Number 5), ("BottomRight", Runtime.Toggle)]) []
                    `shouldCompileTo` [Runtime.MoveTo (Just $ Runtime.Number 5) (Just $ Runtime.Number 10) Runtime.BottomRight]

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
        AST.Call
            "vcenter"
            M.empty
            [ AST.Call "vspace" (M.fromList [("lines", Runtime.Number 5)]) []
            , AST.Literal "hello"
            , AST.Newline
            ]
            `shouldCompileTo` [ Runtime.VCenter 6
                              , Runtime.VSpace 5
                              , Runtime.Output "hello"
                              , Runtime.Newline
                              ]

    it "complies 'vspace'" $ do
        AST.Call "vspace" M.empty [] `shouldCompileTo` [Runtime.VSpace 1]
        AST.Call "vspace" (M.fromList [("lines", Runtime.Number 5)]) [] `shouldCompileTo` [Runtime.VSpace 5]

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

    describe "'img'" $ do
        it "generates an instruction to show the image" $
            AST.Call "image" (M.fromList [("path", Runtime.Filepath "path.png")]) []
                `shouldCompileTo` [Runtime.Exec $ "kitten icat --align center " <> "path.png"]

    describe "'exec'" $ do
        it "generates an instruction to execute a command" $
            AST.Call "exec" (M.fromList [("cmd", Runtime.Literal "cmd and args")]) []
                `shouldCompileTo` [Runtime.Exec "cmd and args"]
        it "resets the terminal for interactive programs" $
            AST.Call
                "exec"
                ( M.fromList
                    [ ("cmd", Runtime.Literal "cmd")
                    , ("interactive", Runtime.Toggle)
                    ]
                )
                []
                `shouldCompileTo` [ Runtime.Exec "cmd"
                                  , Runtime.Reset
                                  ]

    describe "'quote'" $ do
        it "compiles quotes without an author" $ do
            AST.Call "quote" M.empty [AST.Literal "a quote"]
                `shouldCompileTo` [ Runtime.Center 9
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
                    `shouldCompileTo` [ Runtime.Center qlen
                                      , Runtime.Output "“"
                                      , Runtime.Output q
                                      , Runtime.Output "”"
                                      , Runtime.Newline
                                      , Runtime.Center qlen
                                      , Runtime.Output $ padding <> "- " <> citation
                                      ]

        it "styles the quotation marks" $
            AST.Call "quote" (M.fromList [("altColor", Runtime.RGB 127 127 127)]) [AST.Literal "a quote"]
                `shouldCompileTo` [ Runtime.Center 9
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
                `shouldCompileTo` [ Runtime.Center 17
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

nostyle :: Runtime.Style
nostyle = Runtime.emptyStyle

shouldCompileTo :: AST.Expr -> [Runtime.Instruction] -> Expectation
shouldCompileTo expr is = compileExpression expr `shouldBe` Right (V.fromList is)
