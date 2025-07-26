module PDL.Types where

import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import PrettyPrint (Empty, PrettyPrint (..))

newtype Presentation = Presentation {presentation :: [TopLevelExpr]}
    deriving newtype (Show, Eq, Semigroup, Empty, PrettyPrint)

data TopLevelExpr
    = PDefinition Definition
    | PSlide Args [Expr]
    deriving stock (Show, Eq)

instance PrettyPrint TopLevelExpr where
    prettyIndent i (PDefinition _) = i <> "<definition> not implemented"
    prettyIndent i (PSlide _ es) = "Slide:\n" <> indentShow i es <> "\n"

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

instance PrettyPrint Expr where
    prettyIndent i (Literal l) = i <> ".literal " <> T.show l <> "\n"
    prettyIndent i (Instruction name (Args args) es) = i <> "." <> name <> " " <> showArgs <> "\n" <> showSubExprs
      where
        showArgs
            | null args = ""
            | otherwise = "[" <> T.intercalate ", " (pretty <$> args) <> "]"
        showSubExprs
            | null es = ""
            | otherwise = indentShow i es

newtype Args = Args {args :: [Arg]}
    deriving stock (Show, Eq)

data Arg
    = ArgText !Text
    | ArgDuration !Duration
    | ArgHex !Hex
    | ArgInt !Int
    deriving stock (Show, Eq)

instance PrettyPrint Arg where
    pretty (ArgText x) = x
    pretty (ArgDuration x) = pretty x
    pretty (ArgHex x) = pretty x
    pretty (ArgInt x) = T.show x

data Duration
    = Seconds Int
    | Milliseconds Int
    deriving stock (Show, Eq)

instance PrettyPrint Duration where
    pretty (Seconds x) = T.show x <> "s"
    pretty (Milliseconds x) = T.show x <> "ms"

data Hex = Hex (Char, Char) (Char, Char) (Char, Char)
    deriving stock (Show, Eq)

instance PrettyPrint Hex where
    pretty (Hex (r1, r2) (g1, g2) (b1, b2)) = "#" <> T.pack [r1, r2, g1, g2, b1, b2]

fromHex :: Hex -> (Int, Int, Int)
fromHex (Hex r g b) = (fromPair r, fromPair g, fromPair b)
  where
    fromPair :: (Char, Char) -> Int
    fromPair (x, y) = C.digitToInt x * 16 + C.digitToInt y

indentShow :: Text -> [Expr] -> Text
indentShow i es = T.concat (prettyIndent (i <> "  ") <$> es)