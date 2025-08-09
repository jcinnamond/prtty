module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (compileExpressions)
import Compiler.Internal.References (resolveReferences)
import Compiler.Internal.Rewrite (rewrite)
import Compiler.Internal.Types (CompilerState (..), runCompiler)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Parser.AST (Presentation (..))
import Runtime.Instructions (Instruction)
import Runtime.Instructions qualified as Runtime

compile :: [Presentation] -> Either Text (Vector Instruction)
compile ps = do
    let (es, s) = runCompiler (resolveReferences ps >>= rewrite >>= compileExpressions)
    es >>= jumpToStart s >>= addPrelude s

addPrelude :: CompilerState -> Vector Instruction -> Either Text (Vector Instruction)
addPrelude (CompilerState{prelude}) is = pure $ prelude <> is

jumpToStart :: CompilerState -> Vector Instruction -> Either Text (Vector Instruction)
jumpToStart (CompilerState{prelude}) is =
    pure $ case V.findIndex isStartMarker is of
        Nothing -> is
        (Just i) -> V.cons (Runtime.JumpTo $ i + length prelude + 1) is
  where
    isStartMarker :: Instruction -> Bool
    isStartMarker (Runtime.SetMarker "startHere") = True
    isStartMarker _ = False
