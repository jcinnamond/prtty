module Parser.AST (
    Expr (..),
    Presentation (..),
    Number (..),
    Arg (..),
    ArgValue (..),
) where

import Data.Text
import PrettyPrint (Empty, PrettyPrint)
import Runtime.Duration (Duration)

newtype Presentation = Presentation {presentation :: [Expr]}
    deriving newtype (Show, Eq, Semigroup, Empty, PrettyPrint)

data Expr
    = Literal Text
    | Call Text [Arg] [Expr]
    deriving stock (Show, Eq)

data Number
    = Integer Int
    | Rational Int Int
    | Percentage Int
    deriving stock (Show, Eq)

data Arg = Arg Text ArgValue
    deriving stock (Show, Eq)

data ArgValue
    = ArgNumber Int
    | ArgRational Int Int
    | ArgPercentage Int
    | ArgDuration Duration
    | ArgRGB Int Int Int
    | ArgLiteral Text
    deriving stock (Show, Eq)