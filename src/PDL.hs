module PDL (
  pdlParser,
  parseExpr,
  Expr (..),
  parseFile,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parse, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, hspace, printChar, string, symbolChar)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

data Expr
  = Instruction Text [Expr]
  | Literal Text
  deriving stock (Show, Eq)

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [Expr])
parseFile p = do
  s <- TIO.readFile p
  pure $ parse pdlParser p s

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