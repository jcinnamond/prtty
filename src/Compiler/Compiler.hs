module Compiler.Compiler (
    compile,
) where

import Compiler.Internal (compileExpressions)
import Compiler.Internal.References (resolveReferences)
import Compiler.Internal.Rewrite (rewrite)
import Compiler.Internal.Types (CompilerState (..), runCompiler)
import Control.Monad.Error.Class (MonadError (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Options (Options (..))
import Parser.AST (Presentation (..))
import Runtime.Instructions (Instruction)
import Runtime.Instructions qualified as Runtime

compile :: Options -> [Presentation] -> Either Text (Vector Instruction)
compile o ps = do
    let (es, s) = runCompiler (resolveReferences ps >>= rewrite >>= compileExpressions)
    es >>= jump o s >>= addPrelude s

addPrelude :: CompilerState -> Vector Instruction -> Either Text (Vector Instruction)
addPrelude (CompilerState{prelude}) is = pure $ prelude <> is

jump :: Options -> CompilerState -> Vector Instruction -> Either Text (Vector Instruction)
jump o (CompilerState{prelude}) is =
    case T.pack <$> o.startAt of
        Nothing -> pure is
        (Just marker) -> case V.findIndex (findMarker marker) is of
            Nothing -> throwError $ "can't find waypoint marker " <> marker
            (Just i) -> pure $ V.cons (Runtime.JumpTo $ i + length prelude + 1) is
  where
    findMarker :: Text -> Instruction -> Bool
    findMarker x (Runtime.SetMarker y)
        | x == y = True
        | otherwise = False
    findMarker _ _ = False
