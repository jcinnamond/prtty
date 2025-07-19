module PDL (
  pdlParser,
  parseExpr,
  Expr (..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, hspace, printChar, string, symbolChar)

type Parser = Parsec Void Text

data Expr
  = Instruction Text [Expr]
  | Literal Text
  deriving stock (Show, Eq)

pdlParser :: Parser [Expr]
pdlParser = many (parseExpr <* many eol) <* eof

parseExpr :: Parser Expr
parseExpr = parseInstruction <|> parseInlineLiteral

parseInstruction :: Parser Expr
parseInstruction = do
  identifier <- string "." *> parseIdentifier <* hspace
  args <- many parseExpr
  pure $ Instruction identifier args

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> many (alphaNumChar <|> symbolChar)

parseInlineLiteral :: Parser Expr
parseInlineLiteral = Literal . T.pack <$> some printChar

-- pComplexItem :: Parser (String, [String])
-- pComplexItem = L.indentBlock scn p
--   where
--     p = do
--       header <- pItem
--       return (L.IndentMany Nothing (return . (header, )) pLineFold)

-- pLineFold :: Parser String
-- pLineFold = L.lineFold scn $ \sc' ->
--   let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
--   in unwords <$> ps <* scn -- (1)

-- scn :: Parser ()
-- scn = L.space space1 lineComment empty

-- sc :: Parser ()
-- sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- lineComment :: Parser ()
-- lineComment = L.skipLineComment "--"