module Parser.Parser (
    parseFiles,
) where

import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Parser.AST (Presentation (..))
import Parser.Internal (presentation)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

parseFiles :: [FilePath] -> IO (Either Text [Presentation])
parseFiles paths = do
    results <- traverse parseFile paths
    pure $ case lefts results of
        [] -> Right $ rights results
        errs -> Left $ T.intercalate "\n" $ T.pack . errorBundlePretty <$> errs

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Presentation)
parseFile p = do
    s <- TIO.readFile p
    pure $ parse Parser.Internal.presentation p s
