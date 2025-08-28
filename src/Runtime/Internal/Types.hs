module Runtime.Internal.Types where

import Control.Applicative ((<|>))
import Control.Monad.State (StateT, evalStateT)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import PrettyPrint (PrettyPrint (..))
import Runtime.Value (Duration, Value)
import System.Console.Terminal.Size qualified as TSize

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

type Runtime' a = StateT Environment IO a
type Runtime = Runtime' ()

evalRuntime :: Runtime' a -> Environment -> IO a
evalRuntime = evalStateT

data Environment = Environment
    { instructions :: Vector Instruction
    , pc :: !Int
    -- ^ "program counter", a pointer to the next instruction to execute
    , backMarkers :: [Int]
    -- ^ a list of points to jump back to
    , lastJump :: !(Maybe Int)
    -- ^ the last instruction we jumped back to, used to support pressing 'b' multiple times
    , height :: !Int
    -- ^ the height of the terminal, in characters
    , width :: !Int
    -- ^ the width of the terminal, in characters
    , topMargin :: !Int
    , leftMargin :: !Int
    , currentStyle :: !Style
    , styleHistory :: ![Style]
    }
    deriving stock (Show, Eq)

newEnvironment :: Vector Instruction -> IO Environment
newEnvironment is = do
    size <- TSize.size
    let (theight, twidth) = case size of
            Just (TSize.Window{height, width}) -> (height, width)
            Nothing -> (80, 25)
    pure $
        Environment
            { instructions = is
            , pc = 0
            , backMarkers = []
            , lastJump = Nothing
            , height = theight
            , width = twidth
            , topMargin = 0
            , leftMargin = 0
            , currentStyle = emptyStyle
            , styleHistory = []
            }
