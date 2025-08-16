module Runtime.Run (run) where

import Data.Vector (Vector)
import Runtime.Internal.Primitive qualified as Primitive
import Runtime.Internal.Run qualified as Internal
import Runtime.Internal.Types (Instruction (..), newEnvironment)

run :: Vector Instruction -> IO ()
run is = do
    Primitive.initializeTerminal
    Internal.run =<< newEnvironment is
    Primitive.restoreTerminal
