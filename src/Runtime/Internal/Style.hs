module Runtime.Internal.Style where

import Control.Monad.State (modify)
import Control.Monad.State.Class (gets)
import Runtime.Internal.Primitive qualified as Primitive
import Runtime.Internal.Types (Environment (..), Runtime, Style (..))
import Runtime.Value (Value (..))
import VT qualified

-- | Apply a style by sending control codes to the terminal.
apply :: Style -> Runtime
apply s = Primitive.out $ styleFgColor s.fgColor <> styleBgColor s.bgColor <> styleBold s.bold <> styleItalic s.italic
  where
    styleFgColor Nothing = ""
    styleFgColor (Just (RGB r g b)) = VT.fgColor r g b
    styleFgColor _ = error "unimplemented"

    styleBgColor Nothing = ""
    styleBgColor (Just (RGB r g b)) = VT.bgColor r g b
    styleBgColor _ = error "unimplemented"

    styleBold Nothing = VT.resetBold
    styleBold (Just Toggle) = VT.bold
    styleBold _ = error "unimplemented"

    styleItalic Nothing = VT.resetItalic
    styleItalic (Just Toggle) = VT.italic
    styleItalic _ = error "unimplemented"

-- | Set a new style as the current style.
set :: Style -> Runtime
set s = apply s >> modify (\e -> e{currentStyle = e.currentStyle <> s})

-- | Reapply the current style
reapply :: Runtime
reapply = gets currentStyle >>= apply

-- | Save the current style so that it can be restored later.
save :: Runtime
save = modify (\e -> e{styleHistory = e.currentStyle : e.styleHistory})

-- | Restore a previously saved style, if available.
restore :: Runtime
restore = do
    h <- gets styleHistory
    case h of
        [] -> Primitive.nochange
        (s : ss) -> do
            apply s
            modify (\e -> e{currentStyle = s, styleHistory = ss})