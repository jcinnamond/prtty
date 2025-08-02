module Parser.AST (
    Expr (..),
    Presentation (..),
    Number (..),
    Arg (..),
) where

import Data.Text
import PrettyPrint (Empty, PrettyPrint)
import Runtime.Value

newtype Presentation = Presentation {presentation :: [Expr]}
    deriving newtype (Show, Eq, Semigroup, Empty, PrettyPrint)

data Expr
    = Literal Text
    | Newline
    | Call Text [Arg] [Expr]
    deriving stock (Show, Eq)

data Number
    = Integer Int
    | Rational Int Int
    | Percentage Int
    deriving stock (Show, Eq)

data Arg = Arg Text Value
    deriving stock (Show, Eq)
