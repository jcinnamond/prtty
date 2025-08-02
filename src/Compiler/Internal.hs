module Compiler.Internal where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Parser.AST qualified as AST
import Runtime.Duration qualified as Duration
import Runtime.Instructions (Instruction)
import Runtime.Instructions qualified as Runtime
import VT qualified

type Compiler = Either Text (Vector Instruction)

compileExpressions :: [AST.Expr] -> Compiler
compileExpressions exprs = V.concat <$> mapM compileExpression exprs

compileExpression :: AST.Expr -> Compiler
compileExpression (AST.Literal t) = pure $ V.singleton $ Runtime.Output t
compileExpression AST.Newline = pure $ V.singleton Runtime.Newline
compileExpression (AST.Call name args body) = compileBuiltin name args body

compileBuiltin :: Text -> [AST.Arg] -> [AST.Expr] -> Compiler
compileBuiltin "clear" = standalone "clear" compileClear
compileBuiltin "home" = standalone "home" $ V.singleton Runtime.Home
compileBuiltin "margin" = compileMargin
compileBuiltin "nl" = standalone "nl" $ V.singleton $ Runtime.Output "\n"
compileBuiltin "wait" = standalone "wait" compileWait
compileBuiltin "center" = withNoArgs "center" compileCenter
compileBuiltin "vcenter" = withNoArgs "vcenter" compileVCenter
compileBuiltin "type" = compileType
compileBuiltin x = unrecognised x

compileType :: [AST.Arg] -> [AST.Expr] -> Compiler
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
    getPause = case M.lookup "delay" ma of
        Nothing -> pure $ V.singleton $ Runtime.Pause $ Duration.Milliseconds 50
        (Just (AST.ArgDuration x)) -> pure $ V.singleton $ Runtime.Pause x
        _ -> Left "'type' only accepts a duration argument"

    ma :: Map Text AST.ArgValue
    ma = M.fromList $ map (\(AST.Arg name v) -> (name, v)) args
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
    height (AST.Call _ _ body : xs) = height body + height xs

compileClear :: Vector Instruction
compileClear = V.fromList [Runtime.StoreBackMarker, Runtime.Output VT.clear, Runtime.Home]

compileWait :: Vector Instruction
compileWait = V.singleton Runtime.WaitForInput

compileMargin :: [AST.Arg] -> [AST.Expr] -> Compiler
compileMargin args [] = do
    left <- margin "left" Runtime.SetLeftMargin
    top <- margin "top" Runtime.SetTopMargin
    let combined = left <> top
    if V.null combined
        then Left "no margins provided"
        else pure combined
  where
    margin :: Text -> (Runtime.Numerical -> Instruction) -> Compiler
    margin k f = case M.lookup k ma of
        Nothing -> pure V.empty
        Just (AST.ArgNumber x) -> pure $ V.singleton $ f $ Runtime.Number x
        Just (AST.ArgPercentage x) -> pure $ V.singleton $ f $ Runtime.Percent x
        Just (AST.ArgRational n d) -> pure $ V.singleton $ f $ Runtime.Rational n d
        Just v -> Left $ "unsupported left margin type: " <> T.show v

    ma :: Map Text AST.ArgValue
    ma = M.fromList $ map (\(AST.Arg name v) -> (name, v)) args
compileMargin _ body = Left $ "'margin' does not take a body but got: " <> T.show body

standalone :: Text -> Vector Instruction -> [AST.Arg] -> [AST.Expr] -> Compiler
standalone name = withNoArgs name . withNoBody name

withNoArgs :: Text -> ([AST.Expr] -> Compiler) -> [AST.Arg] -> ([AST.Expr] -> Compiler)
withNoArgs _ f [] = f
withNoArgs name _ args = const $ Left $ "unexpected args when calling '" <> name <> "': " <> T.show args

withNoBody :: Text -> Vector Instruction -> [AST.Expr] -> Compiler
withNoBody _ f [] = pure f
withNoBody name _ body = Left $ "unexpected args when calling '" <> name <> "': " <> T.show body

compileWithBody :: Instruction -> [AST.Expr] -> Compiler
compileWithBody i [] = pure $ V.singleton i
compileWithBody i body = V.cons i <$> compileExpressions body

unrecognised :: Text -> [AST.Arg] -> [AST.Expr] -> Compiler
unrecognised name args body = Left $ "unrecognised '" <> name <> "': " <> T.show args <> " {" <> T.show body <> "}"