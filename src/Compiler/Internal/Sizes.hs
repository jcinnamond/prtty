module Compiler.Internal.Sizes where

import Data.Text qualified as T
import Parser.AST qualified as AST

width :: [AST.Expr] -> Int
width [] = 0
width (AST.Newline : xs) = width xs
width (AST.Literal l : xs) = T.length l + width xs
width (AST.LiteralLine l : xs) = T.length l + width xs
width (AST.Call _ _ body : xs) = width body + width xs
