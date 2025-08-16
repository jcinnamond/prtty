{- |
Description : Additional output functions
-}
module Runtime.Internal.IO where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import Runtime.Internal.Navigation qualified as Navigation
import Runtime.Internal.Positioning (moveToLeftMargin)
import Runtime.Internal.Primitive (Keypress (..))
import Runtime.Internal.Primitive qualified as Primitive
import Runtime.Internal.Types (Environment (..), Runtime, Runtime')
import VT qualified

-- | Move to the next line, adjusted for the margin.
newline :: Runtime
newline = Primitive.out "\n" >> moveToLeftMargin

{- | Leave a vertical space.
Moves the cursor down, adjusting for the margin.
-}
vspace :: Int -> Runtime
vspace x = Primitive.out (VT.moveDown x) >> moveToLeftMargin

{- | Block until a keypress is received.
This also handles pageup, which is a multi-character input in a terminal.
-}
waitForInput :: Runtime
waitForInput = do
    k <- Primitive.waitForKeypress
    case k of
        PageUp -> Navigation.goBack
        Key 'b' -> Navigation.goBack
        Key 'q' -> Navigation.end
        Key '.' -> Navigation.end
        Key ':' -> prompt >>= Navigation.jumpToMarker
        _ -> Primitive.nochange

-- | Prompt a user for additional input
prompt :: Runtime' Text
prompt = do
    height <- gets height
    Primitive.out $ VT.moveTo height 0 <> VT.eraseLine <> ": "
    liftIO Primitive.getLine
