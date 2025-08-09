module Compiler.Internal.Types where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, evalState, lift, modify, runState)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Runtime.Instructions (Instruction)

newtype CompilerState = CompilerState
    { prelude :: Vector Instruction
    }
    deriving newtype (Show, Eq)

type Compiler' a = ExceptT Text (State CompilerState) a
type Compiler = Compiler' (Vector Instruction)

evalCompiler :: Compiler' a -> Either Text a
evalCompiler c = evalState (runExceptT c) CompilerState{prelude = V.empty}

runCompiler :: Compiler' a -> (Either Text a, CompilerState)
runCompiler c = runState (runExceptT c) CompilerState{prelude = V.empty}

setPrelude :: Vector Instruction -> Compiler' ()
setPrelude is = lift $ modify (\s -> s{prelude = s.prelude <> is})