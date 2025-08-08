module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (Compiler, compileExpressions)
import Compiler.Internal.References (resolveReferences)
import Parser.AST (Presentation (..))

compile :: [Presentation] -> Compiler
compile ps = resolveReferences ps >>= compileExpressions
