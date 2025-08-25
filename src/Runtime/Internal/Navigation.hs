{- |
Description : Handle instructions for navigating within a presentation.

Navigation refers to jumping to specific instructions within a presentation,
either by specifying a specific address or by searching for a named marker.
-}
module Runtime.Internal.Navigation where

import Control.Monad.State (MonadState (get), StateT, gets, modify)
import Data.Maybe (fromMaybe)
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

{- | Jump back in the presentation to the previous marker.

If the previous jump back was to the previous marker then move the one prior
to that. This stops the navigation getting stuck jumping back and then advancing
by a few instructions.

If there is no marker to jump back to then do nothing.
-}
goBack :: Runtime
goBack = do
    env <- get
    let end = max 1 $ fromMaybe env.pc env.lastJump
    let lastMarker = V.findIndexR isBackMarker $ V.slice 0 (end - 1) env.instructions
    case lastMarker of
        (Just i) -> do
            modify (\e -> e{lastJump = Just i})
            jump $ i + 1
        Nothing -> modify (\e -> e{pc = 0, lastJump = Just 0})
  where
    isBackMarker :: Instruction -> Bool
    isBackMarker StoreBackMarker = True
    isBackMarker _ = False

{- | StoreBackMarker is used to mark locations in the presentation for jumping
back, typically by pressing 'b' or 'PgUp' when waiting for input.

It is mostly ignored by the runtime until trying to decide where to jump back
to, but when encountered it clears any previously stored last jump location.
-}
storeBackMarker :: Runtime
storeBackMarker = modify (\e -> e{lastJump = Nothing})