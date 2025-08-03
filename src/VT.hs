{-
Useful reference: https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
-}
module VT (
    -- Control
    clear,
    moveTo,
    altBuffer,
    noAltBuffer,
    hideCursor,
    showCursor,
    moveToCol,
    moveRight,
    moveDown,
    -- Styles
    bold,
    fgColor,
    bgColor,
    resetBold,
)
where

import Data.Text (Text)
import Data.Text qualified as T

esc :: Text
esc = "\x1b["

clear :: Text
clear = esc <> "2J"

moveTo :: Int -> Int -> Text
moveTo y x = esc <> T.show y <> ";" <> T.show x <> "f"

moveToCol :: Int -> Text
moveToCol y = esc <> T.show y <> "G"

moveRight, moveDown :: Int -> Text
moveRight x = esc <> T.show x <> "C"
moveDown x = esc <> T.show x <> "B"

altBuffer, noAltBuffer :: Text
altBuffer = esc <> "?1049h"
noAltBuffer = esc <> "?1049l"

hideCursor, showCursor :: Text
hideCursor = esc <> "?25l"
showCursor = esc <> "?25h"

bold, resetBold :: Text
bold = esc <> "1m"
resetBold = esc <> "22m"

fgColor, bgColor :: Int -> Int -> Int -> Text
fgColor r g b = esc <> "38;2;" <> T.show r <> ";" <> T.show g <> ";" <> T.show b <> "m"
bgColor r g b = esc <> "48;2;" <> T.show r <> ";" <> T.show g <> ";" <> T.show b <> "m"
