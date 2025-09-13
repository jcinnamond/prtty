{- |
Description : Additional output functions

This needs to be separate from Runtime.Interal.IO to avoid
circular dependencies.
-}
module Runtime.Internal.Output where

import Runtime.Internal.IO (out)
import Runtime.Internal.Positioning (moveToLeftMargin)
import Runtime.Internal.Types (Runtime)
import VT qualified

-- | Move to the next line, adjusted for the margin.
newline :: Runtime
newline = out "\n" >> moveToLeftMargin

{- | Leave a vertical space.
Moves the cursor down, adjusting for the margin.
-}
vspace :: Int -> Runtime
vspace x = out (VT.moveDown x) >> moveToLeftMargin