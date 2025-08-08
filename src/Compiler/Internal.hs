module Compiler.Internal where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Instructions (Instruction)
import Runtime.Instructions qualified as Runtime
import Runtime.Value (Value)
import Runtime.Value qualified as Runtime
import VT qualified

type Compiler = Either Text (Vector Instruction)

compileExpressions :: [AST.Expr] -> Compiler
compileExpressions exprs = V.concat <$> traverse compileExpression exprs

compileExpression :: AST.Expr -> Compiler
compileExpression (AST.Literal t) = pure $ V.singleton $ Runtime.Output t
compileExpression (AST.LiteralLine t) = pure $ V.fromList [Runtime.Output t, Runtime.Newline]
compileExpression AST.Newline = pure $ V.singleton Runtime.Newline
compileExpression (AST.Call name args body) = compileBuiltin name args body

compileBuiltin :: Text -> AST.Args -> [AST.Expr] -> Compiler
compileBuiltin "clear" = standalone "clear" compileClear
compileBuiltin "margin" = compileMargin
compileBuiltin "nl" = standalone "nl" $ V.singleton $ Runtime.Output "\n"
compileBuiltin "wait" = standalone "wait" compileWait
compileBuiltin "home" = standalone "home" $ V.singleton Runtime.Home
compileBuiltin "moveTo" = compileMoveTo
compileBuiltin "center" = withNoArgs "center" compileCenter
compileBuiltin "vcenter" = withNoArgs "vcenter" compileVCenter
compileBuiltin "vspace" = compileVSpace
compileBuiltin "type" = compileType
compileBuiltin "style" = compileStyle
compileBuiltin "slide" = withNoArgs "slide" compileSlide
compileBuiltin "exec" = compileExec
compileBuiltin "image" = compileImage
compileBuiltin "quote" = compileQuote
compileBuiltin "list" = compileList
compileBuiltin "backspace" = compileBackspace
compileBuiltin "alternate" = compileAlternate
compileBuiltin x = unrecognised x

compileAlternate :: AST.Args -> [AST.Expr] -> Compiler
compileAlternate args body = do
    literals <- getLiterals body
    compileExpressions $ interleave (toType literals) (toDelete $ take (length literals - 1) literals)
  where
    getLiterals :: [AST.Expr] -> Either Text [Text]
    getLiterals [] = pure []
    getLiterals (AST.Literal t : rest) = (t :) <$> getLiterals rest
    getLiterals e = Left $ "cannot use " <> T.show e <> " as an alternate"

    toType :: [Text] -> [AST.Expr]
    toType = map (\t -> AST.Call "type" delay [AST.Literal t])

    toDelete :: [Text] -> [AST.Expr]
    toDelete = map (\t -> AST.Call "backspace" (M.insert "count" (Runtime.Number $ T.length t) delay) [])

    interleave :: [AST.Expr] -> [AST.Expr] -> [AST.Expr]
    interleave (x : xs) (y : ys) = x : AST.Call "wait" M.empty [] : y : interleave xs ys
    interleave [] [] = []
    interleave xs [] = xs
    interleave [] ys = ys

    delay :: AST.Args
    delay = case M.lookup "delay" args of
        Nothing -> M.empty
        (Just v) -> M.fromList [("delay", v)]

compileBackspace :: AST.Args -> [AST.Expr] -> Compiler
compileBackspace args body = case M.lookup "count" args of
    Nothing -> pure $ backspace $ width body
    (Just (Runtime.Number c)) -> pure $ backspace c
    x -> Left $ "invalid count given to backspace " <> T.show x
  where
    backspace :: Int -> Vector Instruction
    backspace 0 = V.empty
    backspace x =
        V.fromList
            [ Runtime.Output $ VT.moveLeft 1 <> " " <> VT.moveLeft 1
            , Runtime.Pause pauseDuration
            ]
            <> backspace (x - 1)

    pauseDuration :: Runtime.Duration
    pauseDuration = case M.lookup "delay" args of
        Just (Runtime.Duration d) -> d
        _ -> defaultDelay

compileList :: AST.Args -> [AST.Expr] -> Compiler
compileList args = showItems bullet
  where
    bullet :: Vector Instruction
    bullet = case M.lookup "bullet" args of
        (Just (Runtime.Literal t)) -> V.singleton $ Runtime.Output $ t <> " "
        _ -> V.empty

    showItems :: Vector Instruction -> [AST.Expr] -> Compiler
    showItems _ [] = pure V.empty
    showItems b (h : t) = do
        item <- compileExpression h
        rest <- showItems bullet t
        let wait = if null t then V.empty else V.singleton Runtime.WaitForInput
        pure $
            b
                <> item
                <> V.singleton Runtime.Newline
                <> wait
                <> rest

compileQuote :: AST.Args -> [AST.Expr] -> Compiler
compileQuote _ [] = pure V.empty
compileQuote args body = case M.lookup "citation" args of
    Nothing ->
        compileCenter $ style "“" <> body <> style "”"
    (Just (Runtime.Literal cite)) -> do
        let plen = width body - T.length cite
            padding = T.replicate plen " "
        q <- compileQuote (M.delete "citation" args) body
        styledCitation <- compileExpressions $ style $ padding <> "- " <> cite
        pure $
            q
                <> V.fromList
                    [ Runtime.Newline
                    , Runtime.Center $ width body + 2
                    ]
                <> styledCitation
    _ -> Left "invalid citation"
  where
    style :: Text -> [AST.Expr]
    style t = case altColor of
        Nothing -> [AST.Literal t]
        (Just arg) -> [AST.Call "style" (M.fromList [("fg", arg)]) [AST.Literal t]]

    altColor :: Maybe Value
    altColor = M.lookup "altColor" args

compileMoveTo :: AST.Args -> [AST.Expr] -> Compiler
compileMoveTo args body = do
    anchor <- getAnchor
    let x = M.lookup "x" args
        y = M.lookup "y" args
    withNoBody "moveTo" (V.singleton $ Runtime.MoveTo y x anchor) body
  where
    getAnchor :: Either Text Runtime.Anchor
    getAnchor = case M.lookup "anchor" args of
        (Just (Runtime.Literal "BottomRight")) -> pure Runtime.BottomRight
        (Just (Runtime.Literal "Margin")) -> pure Runtime.Margin
        (Just (Runtime.Literal "TopLeft")) -> pure Runtime.TopLeft
        Nothing -> pure Runtime.Margin
        v -> Left $ "unrecognised anchor: " <> T.show v

compileExec :: AST.Args -> [AST.Expr] -> Compiler
compileExec args body = case M.lookup "cmd" args of
    Nothing -> Left "missing cmd"
    (Just (Runtime.Literal cmd)) -> withNoBody "exec" (V.singleton (Runtime.Exec cmd) <> maybeReset) body
    _ -> Left "malformed exec instruction"
  where
    maybeReset :: Vector Instruction
    maybeReset = maybe V.empty (const $ V.singleton Runtime.Reset) (M.lookup "interactive" args)

compileImage :: AST.Args -> [AST.Expr] -> Compiler
compileImage args body = case M.lookup "path" args of
    Nothing -> Left "missing image path"
    (Just (Runtime.Filepath path)) -> withNoBody "image" (image path) body
    _ -> Left "malformed image instruction"
  where
    image :: Text -> Vector Instruction
    image path = V.singleton $ Runtime.Exec $ "kitten icat --align center " <> path

compileVSpace :: AST.Args -> [AST.Expr] -> Compiler
compileVSpace args = withNoBody "vspace" vspace
  where
    vspace = V.singleton $ Runtime.VSpace (lineCount $ M.lookup "lines" args)
    lineCount (Just (Runtime.Number x)) = x
    lineCount _ = 1

compileSlide :: [AST.Expr] -> Compiler
compileSlide body = do
    rest <- compileExpressions body
    pure $ compileClear <> rest <> compileWait

compileType :: AST.Args -> [AST.Expr] -> Compiler
compileType _ [] = pure V.empty
compileType args (AST.Literal t : body) = do
    pause <- getPause
    rest <- compileType args body
    pure $
        T.foldl'
            ( \is c ->
                is <> V.cons (Runtime.Output $ T.singleton c) pause
            )
            V.empty
            t
            <> rest
  where
    getPause :: Compiler
    getPause = case M.lookup "delay" args of
        Nothing -> pure $ V.singleton $ Runtime.Pause defaultDelay
        (Just (Runtime.Duration x)) -> pure $ V.singleton $ Runtime.Pause x
        _ -> Left "'type' only accepts a duration argument"
compileType args (AST.LiteralLine t : body) = do
    typeLiteral <- compileType args [AST.Literal t]
    rest <- compileType args body
    pure $ typeLiteral <> V.singleton Runtime.Newline <> rest
compileType args (AST.Call cName cArgs cBody : body) = do
    rest <- compileType args body
    expr <- compileExpression $ AST.Call cName cArgs $ pushToLiteral (\l -> [AST.Call "type" args [l]]) cBody
    pure $ expr <> rest
compileType args (e : body) = do
    rest <- compileType args body
    expr <- compileExpression e
    pure $ expr <> rest

pushToLiteral :: (AST.Expr -> [AST.Expr]) -> [AST.Expr] -> [AST.Expr]
pushToLiteral _ [] = []
pushToLiteral f (l@(AST.Literal _) : body) = f l <> pushToLiteral f body
pushToLiteral f (l@(AST.LiteralLine _) : body) = f l <> pushToLiteral f body
pushToLiteral f (AST.Call name args b : body) = AST.Call name args (pushToLiteral f b) : pushToLiteral f body
pushToLiteral f (e : es) = e : pushToLiteral f es

compileCenter :: [AST.Expr] -> Compiler
compileCenter exprs = compileWithBody (Runtime.Center (width exprs)) exprs

width :: [AST.Expr] -> Int
width [] = 0
width (AST.Newline : xs) = width xs
width (AST.Literal l : xs) = T.length l + width xs
width (AST.LiteralLine l : xs) = T.length l + width xs
width (AST.Call _ _ body : xs) = width body + width xs

compileVCenter :: [AST.Expr] -> Compiler
compileVCenter exprs = compileWithBody (Runtime.VCenter (height exprs)) exprs
  where
    height :: [AST.Expr] -> Int
    height [] = 0
    height (AST.Newline : xs) = 1 + height xs
    height (AST.Literal _ : xs) = height xs
    height (AST.LiteralLine _ : xs) = 1 + height xs
    height (AST.Call "vspace" args _ : xs) = vspace (M.lookup "lines" args) + height xs
      where
        vspace (Just (Runtime.Number x)) = x
        vspace _ = 1
    height (AST.Call _ _ body : xs) = height body + height xs

compileClear :: Vector Instruction
compileClear = V.fromList [Runtime.StoreBackMarker, Runtime.Output VT.clear, Runtime.Home]

compileWait :: Vector Instruction
compileWait = V.singleton Runtime.WaitForInput

compileMargin :: AST.Args -> [AST.Expr] -> Compiler
compileMargin args [] = do
    left <- margin "left" Runtime.SetLeftMargin
    top <- margin "top" Runtime.SetTopMargin
    let combined = left <> top
    if V.null combined
        then Left "no margins provided"
        else pure combined
  where
    margin :: Text -> (Value -> Instruction) -> Compiler
    margin k f = case M.lookup k args of
        Nothing -> pure V.empty
        Just (Runtime.Number x) -> pure $ V.singleton $ f $ Runtime.Number x
        Just (Runtime.Percentage x) -> pure $ V.singleton $ f $ Runtime.Percentage x
        Just (Runtime.Rational n d) -> pure $ V.singleton $ f $ Runtime.Rational n d
        Just v -> Left $ "unsupported left margin type: " <> T.show v
compileMargin _ body = Left $ "'margin' does not take a body but got: " <> T.show body

compileStyle :: AST.Args -> [AST.Expr] -> Compiler
compileStyle args body = do
    let style =
            Runtime.emptyStyle
                { Runtime.bold = M.lookup "bold" args
                , Runtime.fgColor = M.lookup "fg" args
                , Runtime.bgColor = M.lookup "bg" args
                }
    if
        | Runtime.nullStyle style -> pure V.empty
        | null body -> pure $ V.singleton $ Runtime.SetStyle style
        | otherwise -> do
            rest <- compileExpressions body
            pure $ V.fromList [Runtime.SaveStyle, Runtime.SetStyle style] <> rest <> V.singleton Runtime.RestoreStyle

defaultDelay :: Runtime.Duration
defaultDelay = Runtime.Milliseconds 50

standalone :: Text -> Vector Instruction -> AST.Args -> [AST.Expr] -> Compiler
standalone name = withNoArgs name . withNoBody name

withNoArgs :: Text -> ([AST.Expr] -> Compiler) -> AST.Args -> ([AST.Expr] -> Compiler)
withNoArgs name f args
    | M.null args = f
    | otherwise = const $ Left $ "unexpected args when calling '" <> name <> "': " <> T.show args

withNoBody :: Text -> Vector Instruction -> [AST.Expr] -> Compiler
withNoBody _ f [] = pure f
withNoBody name _ body = Left $ "unexpected body when calling '" <> name <> "': " <> T.show body

compileWithBody :: Instruction -> [AST.Expr] -> Compiler
compileWithBody i [] = pure $ V.singleton i
compileWithBody i body = V.cons i <$> compileExpressions body

unrecognised :: Text -> AST.Args -> [AST.Expr] -> Compiler
unrecognised name args body = Left $ "unrecognised '" <> name <> "': " <> T.show args <> " {" <> T.show body <> "}"