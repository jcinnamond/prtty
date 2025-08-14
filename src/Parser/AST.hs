module Parser.AST (
    Expr (..),
    Presentation (..),
    PresentationItem (..),
    Args,
) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import PrettyPrint (Empty, PrettyPrint (..))
import Runtime.Value (Value)
import Runtime.Value qualified as Value

newtype Presentation = Presentation {presentation :: [PresentationItem]}
    deriving newtype (Show, Eq, Semigroup, Empty)

instance PrettyPrint Presentation where
    pretty (Presentation{presentation}) = T.intercalate "\n" (pretty <$> presentation)

data PresentationItem
    = PDef Text Value
    | PExpr Expr
    deriving stock (Show, Eq)

instance PrettyPrint PresentationItem where
    pretty (PDef name v) = "DEFINE " <> name <> "=" <> pretty v
    pretty (PExpr e) = pretty e

data Expr
    = Literal Text
    | LiteralLine Text
    | Newline
    | Call Text Args [Expr]
    deriving stock (Show, Eq)

instance PrettyPrint Expr where
    prettyIndent i (Literal t) = i <> "LITERAL " <> t <> "\n"
    prettyIndent i (LiteralLine t) = i <> "LITERAL LINE " <> t <> "\n"
    prettyIndent i Newline = i <> "NEWLINE" <> "\n"
    prettyIndent i (Call name args body) =
        i
            <> "CALL "
            <> name
            <> " "
            <> prettyArgs args
            <> "\n"
            <> T.concat (prettyIndent (i <> "  ") <$> body)

prettyArgs :: Args -> Text
prettyArgs args
    | M.null args = ""
    | otherwise = "[" <> T.intercalate "; " (M.foldMapWithKey f args) <> "]"
  where
    f :: Text -> Value -> [Text]
    f k Value.Toggle = [k]
    f k v = [k <> "=" <> pretty v]

type Args = Map Text Value
