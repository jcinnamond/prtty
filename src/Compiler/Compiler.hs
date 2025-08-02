module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (Compiler, compileExpression)
import Data.Vector qualified as V
import Parser.AST (Presentation (..))

compile :: Presentation -> Compiler
compile (Presentation{presentation}) = V.concat <$> mapM compileExpression presentation
