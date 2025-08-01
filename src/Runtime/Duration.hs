module Runtime.Duration (
    Duration (..),
) where

data Duration
    = Seconds Int
    | Milliseconds Int
    deriving stock (Show, Eq)