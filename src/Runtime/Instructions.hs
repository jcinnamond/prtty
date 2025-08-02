module Runtime.Instructions (
    Instruction (..),
    Numerical (..),
    RGB (..),
) where

import Data.Text (Text)

data Instruction
    = Output Text
    | StoreBackMarker
    | SetTopMargin Numerical
    | SetLeftMargin Numerical
    | Home
    | WaitForInput
    deriving stock (Show, Eq)

data Numerical
    = Percent Int
    | Number Int
    | Rational Int Int
    deriving stock (Show, Eq)

data RGB = RGB Int Int Int
    deriving stock (Show, Eq)