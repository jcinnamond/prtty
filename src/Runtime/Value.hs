module Runtime.Value (
    Value (..),
    Duration (..),
    nanoseconds,
) where

import Data.Text (Text)

data Value
    = Number Int
    | Rational Int Int
    | Percentage Int
    | Duration Duration
    | RGB Int Int Int
    | Literal Text
    deriving stock (Show, Eq)

data Duration
    = Seconds Int
    | Milliseconds Int
    deriving stock (Show, Eq)

nanoseconds :: Duration -> Int
nanoseconds (Seconds x) = x * 1_000_000
nanoseconds (Milliseconds x) = x * 1_000