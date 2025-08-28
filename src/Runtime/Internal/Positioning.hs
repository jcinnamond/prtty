module Runtime.Internal.Positioning where

import Control.Monad.State (gets)
import Runtime.Internal.Types (Environment (..), Runtime')

findCenter :: Int -> Runtime' Int
findCenter x = do
    w <- gets width
    pure $ w `div` 2 - x `div` 2