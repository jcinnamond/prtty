module Runtime.Instructions (
    Instruction (..),
    Style (..),
) where

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
    | Pause Duration
    deriving stock (Show, Eq)

data Style
    = FgColor Value
    | BgColor Value
    | Bold
    deriving stock (Show, Eq)