module Compiler.Internal.Types where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, evalState, gets, lift, modify, runState)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Runtime.Instructions (Instruction)

data CompilerState = CompilerState
    { prelude :: Vector Instruction
    , slideCount :: Int
    }
    deriving stock (Show, Eq)

type Compiler' a = ExceptT Text (State CompilerState) a
type Compiler = Compiler' (Vector Instruction)

evalCompiler :: Compiler' a -> Either Text a
evalCompiler c = evalState (runExceptT c) initialState

runCompiler :: Compiler' a -> (Either Text a, CompilerState)
runCompiler c = runState (runExceptT c) initialState

setPrelude :: Vector Instruction -> Compiler' ()
setPrelude is = lift $ modify (\s -> s{prelude = s.prelude <> is})

incSlideCount :: Compiler' Int
incSlideCount = do
    c <- gets (succ . slideCount)
    modify (\s -> s{slideCount = c})
    pure c

initialState :: CompilerState
initialState =
    CompilerState
        { prelude = V.empty
        , slideCount = 0
        }