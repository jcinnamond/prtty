module Parser.AST (
    Expr (..),
    Presentation (..),
    PresentationItem (..),
    Args,
) where

import Data.Map (Map)
import Data.Text
import PrettyPrint (Empty, PrettyPrint)
import Runtime.Value

newtype Presentation = Presentation {presentation :: [PresentationItem]}
    deriving newtype (Show, Eq, Semigroup, Empty, PrettyPrint)

data PresentationItem
    = PDef Text Value
    | PExpr Expr
    deriving stock (Show, Eq)

data Expr
    = Literal Text
    | LiteralLine Text
    | Newline
    | Call Text Args [Expr]
    deriving stock (Show, Eq)

type Args = Map Text Value
