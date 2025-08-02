module Parser.AST (
    Expr (..),
    Presentation (..),
    Number (..),
    Args,
) where

import Data.Map (Map)
import Data.Text
import PrettyPrint (Empty, PrettyPrint)
import Runtime.Value

newtype Presentation = Presentation {presentation :: [Expr]}
    deriving newtype (Show, Eq, Semigroup, Empty, PrettyPrint)

data Expr
    = Literal Text
    | Newline
    | Call Text Args [Expr]
    deriving stock (Show, Eq)

data Number
    = Integer Int
    | Rational Int Int
    | Percentage Int
    deriving stock (Show, Eq)

type Args = Map Text Value
