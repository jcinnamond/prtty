module Runtime.Instructions (
    Instruction (..),
    Anchor (..),
    Style (..),
    nullStyle,
    emptyStyle,
) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import PrettyPrint (PrettyPrint (..))
import Runtime.Value (Duration, Value)

data Instruction
    = Output Text
    | Newline
    | StoreBackMarker
    | SetTopMargin Value
    | SetLeftMargin Value
    | WaitForInput
    | Home
    | MoveTo (Maybe Value) (Maybe Value) Anchor -- MoveTo y x
    | VCenter Int
    | Center Int
    | VSpace Int
    | Pause Duration
    | SetStyle Style
    | SaveStyle
    | RestoreStyle
    | Exec Text
    | Reset
    | JumpTo Int
    | SetMarker Text
    deriving stock (Show, Eq)

instance PrettyPrint Instruction where
    pretty (SetTopMargin v) = "SetTopMargin " <> pretty v
    pretty (SetLeftMargin v) = "SetLeftMargin " <> pretty v
    pretty (MoveTo y x anchor) = "MoveTo y=" <> pretty y <> ", x=" <> pretty x <> " anchor=" <> pretty anchor
    pretty (SetStyle s) = "SetStyle " <> pretty s
    pretty (Pause d) = "Pause " <> pretty d
    pretty x = T.show x

data Anchor = TopLeft | BottomRight | Margin
    deriving stock (Show, Eq)

data Style
    = Style
    { fgColor :: Maybe Value
    , bgColor :: Maybe Value
    , bold :: Maybe Value
    , italic :: Maybe Value
    }
    deriving stock (Show, Eq)

instance PrettyPrint Style where
    pretty (Style{fgColor, bgColor, bold, italic}) =
        T.intercalate
            "; "
            ( catMaybes
                [ prettyColor "fgColor" fgColor
                , prettyColor "bgColor" bgColor
                , prettyAttribute "bold" bold
                , prettyAttribute "italic" italic
                ]
            )

prettyColor :: Text -> Maybe Value -> Maybe Text
prettyColor l m = (\v -> l <> " = " <> pretty v) <$> m

prettyAttribute :: Text -> Maybe Value -> Maybe Text
prettyAttribute l m = l <$ m

instance Semigroup Style where
    a <> b =
        Style
            { fgColor = b.fgColor <|> a.fgColor
            , bgColor = b.bgColor <|> a.bgColor
            , bold = b.bold <|> a.bold
            , italic = b.italic <|> a.italic
            }

emptyStyle :: Style
emptyStyle =
    Style
        { fgColor = Nothing
        , bgColor = Nothing
        , bold = Nothing
        , italic = Nothing
        }

nullStyle :: Style -> Bool
nullStyle s = isNothing s.fgColor && isNothing s.bgColor && isNothing s.bold && isNothing s.italic
