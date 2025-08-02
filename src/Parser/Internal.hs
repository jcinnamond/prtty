module Parser.Internal where

import Data.Char (isPrint, isSpace)
import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Parser.AST (Arg (..), ArgValue (..), Expr (..), Presentation (..))
import Runtime.Duration (Duration (..))
import Text.Megaparsec (MonadParsec (notFollowedBy, takeWhile1P), Parsec, between, choice, eof, many, satisfy, sepBy, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, hexDigitChar, hspace, space, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

presentation :: Parser Presentation
presentation = Presentation . concat <$> many expressionChain <* eof

expressionChain :: Parser [Expr]
expressionChain = expression `sepBy1` (hspace <* char '<' <* hspace) <* many eol

expression :: Parser Expr
expression =
    choice
        [ newline
        , call
        , Literal <$> literal
        ]

newline :: Parser Expr
newline = string ".nl" >> pure Newline

call :: Parser Expr
call = L.indentBlock space p
  where
    p = do
        name <- char '.' *> identifier <* hspace
        as <- args <* hspace
        inlineBody <- (string "<<" *> hspace *> expressionChain) <|> pure []
        pure $ L.IndentMany Nothing (\x -> pure $ Call name as (inlineBody <> concat x)) expressionChain

identifier :: Parser Text
identifier = T.pack <$> some alphaNumChar

args :: Parser [Arg]
args = matchArgs <|> pure []
  where
    matchArgs :: Parser [Arg]
    matchArgs =
        between
            (char '[')
            (char ']')
            (matchArg `sepBy` char ';')

    matchArg :: Parser Arg
    matchArg = do
        name <- hspace *> identifier <* hspace
        _ <- char '=' <* hspace
        v <-
            choice
                [ try $ uncurry ArgRational <$> try rational
                , try $ ArgPercentage <$> percentage
                , try $ ArgDuration <$> duration
                , uncurry3 ArgRGB <$> rgb
                , ArgNumber <$> L.decimal
                , ArgLiteral <$> identifier
                ]
        _ <- hspace
        pure $ Arg name v

    uncurry3 :: forall a b c d. (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z

rational :: Parser (Int, Int)
rational = do
    n <- L.decimal
    _ <- hspace <* char '/' <* hspace
    d <- L.decimal
    pure (n, d)

percentage :: Parser Int
percentage = L.decimal <* hspace <* char '%'

duration :: Parser Duration
duration =
    choice
        [ try $ Seconds <$> L.decimal <* hspace <* char 's'
        , Milliseconds <$> L.decimal <* hspace <* string "ms"
        ]

rgb :: Parser (Int, Int, Int)
rgb = do
    _ <- char '#'
    r1 <- hexDigitChar
    r2 <- hexDigitChar
    let r = fromPair r1 r2
    g1 <- hexDigitChar
    g2 <- hexDigitChar
    let g = fromPair g1 g2
    b1 <- hexDigitChar
    b2 <- hexDigitChar
    let b = fromPair b1 b2
    pure (r, g, b)
  where
    fromPair :: Char -> Char -> Int
    fromPair x y = C.digitToInt x * 16 + C.digitToInt y

literal :: Parser Text
literal = nonSpace <> literalP

nonSpace :: Parser Text
nonSpace = takeWhile1P (Just "non space") isNonSpace
  where
    isNonSpace x = isPrint x && not (isSpace x) && x /= '<'

literalP :: Parser Text
literalP =
    T.pack
        <$> many
            ( choice
                [ satisfy (\x -> isPrint x && (x /= '>') && (x /= '<'))
                , try $ char '>' <* notFollowedBy (char '>')
                , try $ char '<' <* notFollowedBy (char '<')
                ]
            )
