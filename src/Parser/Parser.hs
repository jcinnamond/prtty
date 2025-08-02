module Parser.Parser (
    parseFiles,
) where

import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Parser.AST (Presentation (..))
import Parser.Internal (presentation)
import Text.Megaparsec (ParseErrorBundle, parse)

parseFiles :: [FilePath] -> IO (Either [ParseErrorBundle Text Void] Presentation)
parseFiles ps = do
    res <- mapM parseFile ps
    pure $ case lefts res of
        [] -> Right $ collapse $ rights res
        errs -> Left errs

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Presentation)
parseFile p = do
    s <- TIO.readFile p
    pure $ parse Parser.Internal.presentation p s

collapse :: [Presentation] -> Presentation
collapse [] = Presentation []
collapse (p : ps) = foldl (<>) p ps
