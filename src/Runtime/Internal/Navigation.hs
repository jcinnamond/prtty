{- |
Description : Handle instructions for navigating within a presentation.

Navigation refers to jumping to specific instructions within a presentation,
either by specifying a specific address or by searching for a named marker.
-}
module Runtime.Internal.Navigation where

import Control.Monad.State (StateT, gets, modify)
import Data.Text (Text)
import Data.Vector qualified as V
import Runtime.Internal.Types (Environment (..), Instruction (..), Runtime)

{- | Jump to a given instruction.
The presentation will continue from that point.
-}
jump :: Int -> Runtime
jump i = modify (\e -> e{pc = i})

{- | Jump to a named marker.
The presentation will continue from that point.

If the marker can't be found the presentation silently continues from the
current location.
-}
jumpToMarker :: Text -> Runtime
jumpToMarker marker = do
    location <- findMarker marker
    maybe (pure ()) jump location

{- | Search for a named marker within the presentation, returning the
instruction address if it is found.
-}
findMarker :: Text -> StateT Environment IO (Maybe Int)
findMarker marker = do
    gets (V.findIndex (find marker) . instructions)
  where
    find :: Text -> Instruction -> Bool
    find x (SetMarker y)
        | x == y = True
        | otherwise = False
    find _ _ = False