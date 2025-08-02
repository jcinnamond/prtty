{-
Useful reference: https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
-}
module VT (
    Displayable (..),
    toText,
    -- Styles
    reset,
    -- Colors
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    -- Let's try something new
    -- Control
    clear,
    moveTo,
    altBuffer,
    noAltBuffer,
    hideCursor,
    showCursor,
    moveToCol,
    moveRight,
    -- Styles
    bold,
    fgColor,
    bgColor,
    esc',
    resetBold,
)
where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T

esc' :: Text
esc' = "\x1b["

clear :: Text
clear = esc' <> "2J"

moveTo :: Int -> Int -> Text
moveTo y x = esc' <> fromInt y <> ";" <> fromInt x <> "f"

moveToCol :: Int -> Text
moveToCol y = esc' <> fromInt y <> "G"

moveRight :: Int -> Text
moveRight x = esc' <> fromInt x <> "C"

altBuffer, noAltBuffer :: Text
altBuffer = esc' <> "?1049h"
noAltBuffer = esc' <> "?1049l"

hideCursor, showCursor :: Text
hideCursor = esc' <> "?25l"
showCursor = esc' <> "?25h"

bold, resetBold :: Text
bold = esc' <> "1m"
resetBold = esc' <> "22m"

fgColor, bgColor :: Int -> Int -> Int -> Text
fgColor r g b = esc' <> "38;2;" <> fromInt r <> ";" <> fromInt g <> ";" <> fromInt b <> "m"
bgColor r g b = esc' <> "48;2;" <> fromInt r <> ";" <> fromInt g <> ";" <> fromInt b <> "m"

data Displayable
    = Literal Text
    | Style Text
    | Home
    | MoveTo Int !Int
    | MoveToCol !Int
    | SaveCursor
    | RestoreCursor
    | HideCursor
    | ShowCursor
    | SaveScreen
    | RestoreScreen
    | AltBuffer
    | NoAltBuffer
    | ClearScreen
    | Combined Displayable Displayable
    deriving stock (Show, Eq)

instance IsString Displayable where
    fromString = Literal . T.pack

instance Semigroup Displayable where
    (Literal x) <> (Literal y) = Literal $ x <> y
    (Style x) <> (Style y) = Style $ x <> ";" <> y
    x <> y = Combined x y

esc :: Text -> Text -> Text
esc x end = "\x1b[" <> x <> end

toText :: Displayable -> Text
toText (Literal x) = x
toText (Style x) = esc x "m"
toText Home = esc "" "H"
toText (MoveTo y x) = esc (fromInt y <> ";" <> fromInt x) "f"
toText (MoveToCol x) = esc (fromInt x) "G"
toText SaveCursor = esc "7" ""
toText RestoreCursor = esc "8" ""
toText HideCursor = esc "?25" "l"
toText ShowCursor = esc "?25" "h"
toText SaveScreen = esc "?47" "h"
toText RestoreScreen = esc "?47" "l"
toText ClearScreen = esc "2J" ""
toText (Combined x y) = toText x <> toText y
toText AltBuffer = esc "?1049" "h"
toText NoAltBuffer = esc "?1049" "l"

reset :: Displayable
reset = Style "0"

black, red, green, yellow, blue, magenta, cyan, white :: Displayable
black = Style "30"
red = Style "31"
green = Style "32"
yellow = Style "33"
blue = Style "34"
magenta = Style "35"
cyan = Style "36"
white = Style "37"

hexToStyle :: (Int, Int, Int) -> Displayable
hexToStyle (r, g, b) = toStyle r <> toStyle g <> toStyle b
  where
    toStyle :: Int -> Displayable
    toStyle = Style . fromInt

fromInt :: Int -> Text
fromInt = T.pack . show
