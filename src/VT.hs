{-
Useful reference: https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
-}
module VT (
    Displayable (..),
    toText,
    -- Styles
    reset,
    bold,
    -- Colors
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    fgColor,
    fgRGB,
    bgColor,
    bgRGB,
)
where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T

data Displayable
    = Literal Text
    | Style Text
    | Home
    | MoveTo Int Int
    | SaveCursor
    | RestoreCursor
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
toText (MoveTo x y) = esc (fromInt x <> ";" <> fromInt y) "H"
toText SaveCursor = esc "7" ""
toText RestoreCursor = esc "8" ""
toText SaveScreen = esc "?47" "h"
toText RestoreScreen = esc "?47" "l"
toText ClearScreen = esc "" "J"
toText (Combined x y) = toText x <> toText y
toText AltBuffer = esc "?1049" "h"
toText NoAltBuffer = esc "?1049" "l"

reset :: Displayable
reset = Style "0"

bold :: Displayable
bold = Style "1"

black, red, green, yellow, blue, magenta, cyan, white :: Displayable
black = Style "30"
red = Style "31"
green = Style "32"
yellow = Style "33"
blue = Style "34"
magenta = Style "35"
cyan = Style "36"
white = Style "37"

fgColor :: Text -> Displayable
fgColor x = Style "38" <> Style "5" <> Style x

fgRGB :: Int -> Int -> Int -> Displayable
fgRGB r g b = Style "38" <> Style "2" <> Style (fromInt r) <> Style (fromInt g) <> Style (fromInt b)

bgColor :: Text -> Displayable
bgColor x = Style "48" <> Style "5" <> Style x

bgRGB :: Int -> Int -> Int -> Displayable
bgRGB r g b = Style "48" <> Style "2" <> Style (fromInt r) <> Style (fromInt g) <> Style (fromInt b)

fromInt :: Int -> Text
fromInt = T.pack . show
