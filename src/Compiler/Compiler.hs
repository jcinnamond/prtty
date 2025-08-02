module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (Compiler, compileExpressions)
import Parser.AST (Presentation (..))

compile :: Presentation -> Compiler
compile (Presentation{presentation}) = compileExpressions presentation
