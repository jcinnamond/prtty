module Runtime.Value (
    Value (..),
    Duration (..),
    nanoseconds,
) where

import Data.Text (Text)
import Data.Text qualified as T
import PrettyPrint

data Value
    = Number Int
    | Rational Int Int
    | Percentage Int
    | Duration Duration
    | RGB Int Int Int
    | Literal Text
    | Filepath Text
    | Toggle
    | Reference Text
    deriving stock (Show, Eq)

instance PrettyPrint Value where
    pretty (Number i) = T.show i
    pretty (Rational n d) = T.show n <> "/" <> T.show d
    pretty (Percentage i) = T.show i <> "%"
    pretty (Duration d) = pretty d
    pretty (RGB r g b) = "RGB " <> T.show r <> " " <> T.show g <> " " <> T.show b
    pretty (Literal t) = "\"" <> t <> "\""
    pretty (Filepath t) = t
    pretty Toggle = ""
    pretty (Reference t) = "$" <> t

data Duration
    = Seconds Int
    | Milliseconds Int
    deriving stock (Show, Eq)

instance PrettyPrint Duration where
    pretty (Seconds i) = T.show i <> "s"
    pretty (Milliseconds i) = T.show i <> "ms"

nanoseconds :: Duration -> Int
nanoseconds (Seconds x) = x * 1_000_000
nanoseconds (Milliseconds x) = x * 1_000