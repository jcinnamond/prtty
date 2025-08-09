module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (Compiler, compileExpressions)
import Compiler.Internal.References (resolveReferences)
import Compiler.Internal.Rewrite (rewrite)
import Parser.AST (Presentation (..))

compile :: [Presentation] -> Compiler
compile ps = resolveReferences ps >>= rewrite >>= compileExpressions
