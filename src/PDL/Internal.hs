module PDL.Internal where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import PDL.Types
import Text.Megaparsec (Parsec, between, choice, eof, many, manyTill, sepBy, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, hexDigitChar, hspace, string, symbolChar)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parsePresentation :: Parser Presentation
parsePresentation = Presentation <$> many topLevelExpr <* eof
  where
    topLevelExpr =
        choice
            [ PDefinition <$> parseDefinition
            , parseSlide
            ]

parseDefinition :: Parser Definition
parseDefinition = fail "not implemented"

{-
current thinking for definitions:
\highlight (color : Hex, ...) ->
    bold
    color $color
    ...
-}

parseSlide :: Parser TopLevelExpr
parseSlide = do
    _ <- string "---" <* hspace <* string "slide" <* hspace
    args <- parseArgs
    _ <- eol
    es <- many (parseExpr <* many eol)
    pure $ PSlide args es

parseExpr :: Parser Expr
parseExpr =
    choice
        [ parseInstruction
        , Literal <$> parseTextLiteral
        ]

parseInstruction :: Parser Expr
parseInstruction = do
    identifier <- parseIdentifier <* hspace
    args <- parseArgs <* hspace
    es <- many parseExpr
    pure $ Instruction identifier args es

parseArgs :: Parser Args
parseArgs =
    someArgs
        <|> pure (Args [])
  where
    someArgs :: Parser Args
    someArgs =
        between
            (char '[')
            (char ']')
            (Args <$> parseArg `sepBy` char ';')

    parseArg :: Parser Arg
    parseArg =
        hspace
            *> choice
                [ ArgHex <$> parseHex
                , try $ ArgDuration <$> parseDuration
                , ArgInt <$> parseInt
                , ArgText <$> parseTextLiteral
                ]
            <* hspace

parseHex :: Parser Hex
parseHex = do
    _ <- char '#'
    r1 <- hexDigitChar
    r2 <- hexDigitChar
    g1 <- hexDigitChar
    g2 <- hexDigitChar
    b1 <- hexDigitChar
    b2 <- hexDigitChar
    pure $ Hex (r1, r2) (g1, g2) (b1, b2)

parseInt :: Parser Int
parseInt = L.decimal

parseDuration :: Parser Duration
parseDuration =
    choice
        [ try $ Seconds <$> L.decimal <* string "s"
        , Milliseconds <$> L.decimal <* string "ms"
        ]

parseTextLiteral :: Parser Text
parseTextLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> some (alphaNumChar <|> symbolChar)
