module Runtime.Instructions (
    Instruction (..),
    Style (..),
    nullStyle,
    emptyStyle,
) where

import Control.Applicative ((<|>))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Runtime.Value (Duration, Value)

data Instruction
    = Output Text
    | Newline
    | StoreBackMarker
    | SetTopMargin Value
    | SetLeftMargin Value
    | Home
    | WaitForInput
    | VCenter Int
    | Center Int
    | VSpace Int
    | Pause Duration
    | SetStyle Style
    | SaveStyle
    | RestoreStyle
    deriving stock (Show, Eq)

data Style
    = Style
    { fgColor :: Maybe Value
    , bgColor :: Maybe Value
    , bold :: Maybe Value
    }
    deriving stock (Show, Eq)

instance Semigroup Style where
    a <> b =
        Style
            { fgColor = b.fgColor <|> a.fgColor
            , bgColor = b.bgColor <|> a.bgColor
            , bold = b.bold <|> a.bold
            }

emptyStyle :: Style
emptyStyle =
    Style
        { fgColor = Nothing
        , bgColor = Nothing
        , bold = Nothing
        }

nullStyle :: Style -> Bool
nullStyle s = isNothing s.fgColor && isNothing s.bgColor && isNothing s.bold
