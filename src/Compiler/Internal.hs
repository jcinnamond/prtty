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
compileExpressions exprs = V.concat <$> mapM compileExpression exprs

compileExpression :: AST.Expr -> Compiler
compileExpression (AST.Literal t) = pure $ V.singleton $ Runtime.Output t
compileExpression AST.Newline = pure $ V.singleton Runtime.Newline
compileExpression (AST.Call name args body) = compileBuiltin name args body

compileBuiltin :: Text -> AST.Args -> [AST.Expr] -> Compiler
compileBuiltin "clear" = standalone "clear" compileClear
compileBuiltin "home" = standalone "home" $ V.singleton Runtime.Home
compileBuiltin "margin" = compileMargin
compileBuiltin "nl" = standalone "nl" $ V.singleton $ Runtime.Output "\n"
compileBuiltin "wait" = standalone "wait" compileWait
compileBuiltin "center" = withNoArgs "center" compileCenter
compileBuiltin "vcenter" = withNoArgs "vcenter" compileVCenter
compileBuiltin "vspace" = compileVSpace
compileBuiltin "type" = compileType
compileBuiltin "style" = compileStyle
compileBuiltin "slide" = withNoArgs "slide" compileSlide
compileBuiltin "exec" = compileExec
compileBuiltin "image" = compileImage
compileBuiltin x = unrecognised x

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
compileType args [AST.Literal t] = do
    pause <- getPause
    pure $
        T.foldl'
            ( \is c ->
                is <> V.cons (Runtime.Output $ T.singleton c) pause
            )
            V.empty
            t
  where
    getPause :: Compiler
    getPause = case M.lookup "delay" args of
        Nothing -> pure $ V.singleton $ Runtime.Pause $ Runtime.Milliseconds 50
        (Just (Runtime.Duration x)) -> pure $ V.singleton $ Runtime.Pause x
        _ -> Left "'type' only accepts a duration argument"
compileType _ _ = Left "type only accepts a single literal"

compileCenter :: [AST.Expr] -> Compiler
compileCenter exprs = compileWithBody (Runtime.Center (width exprs)) exprs
  where
    width [] = 0
    width (AST.Newline : xs) = width xs
    width (AST.Literal l : xs) = T.length l + width xs
    width (AST.Call _ _ body : xs) = width body + length xs

compileVCenter :: [AST.Expr] -> Compiler
compileVCenter exprs = compileWithBody (Runtime.VCenter (height exprs)) exprs
  where
    height :: [AST.Expr] -> Int
    height [] = 0
    height (AST.Newline : xs) = 1 + height xs
    height (AST.Literal _ : xs) = height xs
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

standalone :: Text -> Vector Instruction -> AST.Args -> [AST.Expr] -> Compiler
standalone name = withNoArgs name . withNoBody name

withNoArgs :: Text -> ([AST.Expr] -> Compiler) -> AST.Args -> ([AST.Expr] -> Compiler)
withNoArgs name f args
    | M.null args = f
    | otherwise = const $ Left $ "unexpected args when calling '" <> name <> "': " <> T.show args

withNoBody :: Text -> Vector Instruction -> [AST.Expr] -> Compiler
withNoBody _ f [] = pure f
withNoBody name _ body = Left $ "unexpected args when calling '" <> name <> "': " <> T.show body

compileWithBody :: Instruction -> [AST.Expr] -> Compiler
compileWithBody i [] = pure $ V.singleton i
compileWithBody i body = V.cons i <$> compileExpressions body

unrecognised :: Text -> AST.Args -> [AST.Expr] -> Compiler
unrecognised name args body = Left $ "unrecognised '" <> name <> "': " <> T.show args <> " {" <> T.show body <> "}"