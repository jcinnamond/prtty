module PDL.Types where
import Data.Text (Text)

newtype Presentation = Presentation {presentation :: [TopLevelExpr]}
    deriving newtype (Show, Eq, Semigroup)

data TopLevelExpr
    = PDefinition Definition
    | PSlide Args [Expr]
    deriving stock (Show, Eq)

data Definition = Definition Text Parameters [Expr]
    deriving stock (Show, Eq)

newtype Parameters = Parameters {parameters :: [Parameter]}
    deriving newtype (Show, Eq)

data Parameter = Parameter Text PType
    deriving stock (Show, Eq)

data PType
    = PTypeText
    | PTypeDuration
    | PTypeHex
    | PTypeRest
    deriving stock (Show, Eq)

data Expr
    = Instruction Text Args [Expr]
    | Literal Text
    deriving stock (Show, Eq)

newtype Args = Args {args :: [Arg]}
    deriving stock (Show, Eq)

data Arg
    = ArgText Text
    | ArgDuration Duration
    | ArgHex Hex
    deriving stock (Show, Eq)

data Duration
    = Seconds Int
    | Milliseconds Int
    deriving stock (Show, Eq)

data Hex = Hex (Char, Char) (Char, Char) (Char, Char)
    deriving stock (Show, Eq)
