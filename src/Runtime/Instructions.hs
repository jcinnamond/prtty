module Runtime.Instructions (
    Instruction (..),
    Numerical (..),
    RGB (..),
) where

import Data.Text (Text)

data Instruction
    = Output Text
    | Newline
    | StoreBackMarker
    | SetTopMargin Numerical
    | SetLeftMargin Numerical
    | Home
    | WaitForInput
    | VCenter Int
    | Center Int
    deriving stock (Show, Eq)

data Numerical
    = Percent Int
    | Number Int
    | Rational Int Int
    deriving stock (Show, Eq)

data RGB = RGB Int Int Int
    deriving stock (Show, Eq)