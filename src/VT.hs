{-
Useful reference: https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
-}
module VT (
   Displayable(..),
   toText,

   -- Text styles
   bold,

   -- Colors
   black, red, green, yellow, blue, magenta, cyan, white,
   fgColor,
   bgColor,
)
where

import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.Text as T

data Displayable = 
    Literal Text
    | Style Text
    | Reset
    | Combined Displayable Displayable

instance IsString Displayable where
    fromString = Literal . T.pack

instance Semigroup Displayable where
    (Literal x ) <> (Literal y) = Literal $ x <> y
    (Style x) <> (Style y) = Style $ x <> ";" <> y
    x <> y = Combined x y

toText :: Displayable -> Text
toText (Literal x) = x
toText (Style x) = "\x1b[" <> x <> "m"
toText Reset = "\x1b[0m"
toText (Combined x y) = toText x <> toText y

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

bgColor :: Text -> Displayable
bgColor x = Style "48" <> Style "5" <> Style x