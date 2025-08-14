module Compiler.Internal where

import Compiler.Internal.Sizes (width)
import Compiler.Internal.Types (Compiler, Compiler', incSlideCount, setPrelude)
import Control.Monad.Except (MonadError (throwError))
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

compileExpressions :: [AST.Expr] -> Compiler
compileExpressions exprs = V.concat <$> traverse compileExpression exprs

compileExpression :: AST.Expr -> Compiler
compileExpression (AST.Literal t) = pure $ V.singleton $ Runtime.Output t
compileExpression (AST.LiteralLine _) = error "this should have been rewritten"
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
compileBuiltin "backspace" = compileBackspace
compileBuiltin "prelude" = withNoArgs "prelude" compilePrelude
compileBuiltin "waypoint" = compileWaypoint
compileBuiltin x = unrecognised x

compileWaypoint :: AST.Args -> [AST.Expr] -> Compiler
compileWaypoint args body = do
    marker <- setMarker
    withNoBody "waypoint" marker body
  where
    setMarker :: Compiler
    setMarker = case M.lookup "name" args of
        Nothing -> throwError "missing waypoint marker name"
        Just (Runtime.Literal n) -> pure $ V.singleton $ Runtime.SetMarker n
        x -> throwError $ "invalid value for waypoint marker name" <> T.show x

compilePrelude :: [AST.Expr] -> Compiler
compilePrelude body = do
    compileExpressions body >>= setPrelude
    pure V.empty

compileBackspace :: AST.Args -> [AST.Expr] -> Compiler
compileBackspace args body = case M.lookup "count" args of
    Nothing -> pure $ backspace $ width body
    (Just (Runtime.Number c)) -> pure $ backspace c
    x -> throwError $ "invalid count given to backspace " <> T.show x
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

compileMoveTo :: AST.Args -> [AST.Expr] -> Compiler
compileMoveTo args body = do
    anchor <- getAnchor
    let x = M.lookup "x" args
        y = M.lookup "y" args
    withNoBody "moveTo" (V.singleton $ Runtime.MoveTo y x anchor) body
  where
    getAnchor :: Compiler' Runtime.Anchor
    getAnchor = case M.lookup "anchor" args of
        (Just (Runtime.Literal "BottomRight")) -> pure Runtime.BottomRight
        (Just (Runtime.Literal "Margin")) -> pure Runtime.Margin
        (Just (Runtime.Literal "TopLeft")) -> pure Runtime.TopLeft
        Nothing -> pure Runtime.Margin
        v -> throwError $ "unrecognised anchor: " <> T.show v

compileExec :: AST.Args -> [AST.Expr] -> Compiler
compileExec args body = case M.lookup "cmd" args of
    Nothing -> throwError "missing cmd"
    (Just (Runtime.Literal cmd)) -> withNoBody "exec" (V.singleton (Runtime.Exec cmd) <> maybeReset) body
    _ -> throwError "malformed exec instruction"
  where
    maybeReset :: Vector Instruction
    maybeReset = maybe V.empty (const $ V.singleton Runtime.Reset) (M.lookup "interactive" args)

compileVSpace :: AST.Args -> [AST.Expr] -> Compiler
compileVSpace args = withNoBody "vspace" vspace
  where
    vspace = V.singleton $ Runtime.VSpace (lineCount $ M.lookup "lines" args)
    lineCount (Just (Runtime.Number x)) = x
    lineCount _ = 1

compileSlide :: [AST.Expr] -> Compiler
compileSlide body = do
    count <- incSlideCount
    rest <- compileExpressions body
    pure $ marker count <> compileClear <> rest <> compileWait
  where
    marker :: Int -> Vector Instruction
    marker c = V.singleton $ Runtime.SetMarker $ "slide" <> T.show c

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
        _ -> throwError "'type' only accepts a duration argument"
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
    height (AST.Call "list" _ body : xs) = length body + height xs
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
        then throwError "no margins provided"
        else pure combined
  where
    margin :: Text -> (Value -> Instruction) -> Compiler
    margin k f = case M.lookup k args of
        Nothing -> pure V.empty
        Just (Runtime.Number x) -> pure $ V.singleton $ f $ Runtime.Number x
        Just (Runtime.Percentage x) -> pure $ V.singleton $ f $ Runtime.Percentage x
        Just (Runtime.Rational n d) -> pure $ V.singleton $ f $ Runtime.Rational n d
        Just v -> throwError $ "unsupported left margin type: " <> T.show v
compileMargin _ body = throwError $ "'margin' does not take a body but got: " <> T.show body

compileStyle :: AST.Args -> [AST.Expr] -> Compiler
compileStyle args body = do
    let style =
            Runtime.emptyStyle
                { Runtime.bold = M.lookup "bold" args
                , Runtime.italic = M.lookup "italic" args
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
defaultDelay = Runtime.Milliseconds 30

standalone :: Text -> Vector Instruction -> AST.Args -> [AST.Expr] -> Compiler
standalone name = withNoArgs name . withNoBody name

withNoArgs :: Text -> ([AST.Expr] -> Compiler) -> AST.Args -> ([AST.Expr] -> Compiler)
withNoArgs name f args
    | M.null args = f
    | otherwise = const $ throwError $ "unexpected args when calling '" <> name <> "': " <> T.show args

withNoBody :: Text -> Vector Instruction -> [AST.Expr] -> Compiler
withNoBody _ f [] = pure f
withNoBody name _ body = throwError $ "unexpected body when calling '" <> name <> "': " <> T.show body

compileWithBody :: Instruction -> [AST.Expr] -> Compiler
compileWithBody i [] = pure $ V.singleton i
compileWithBody i body = V.cons i <$> compileExpressions body

unrecognised :: Text -> AST.Args -> [AST.Expr] -> Compiler
unrecognised name args body = throwError $ "unrecognised '" <> name <> "': " <> T.show args <> " {" <> T.show body <> "}"