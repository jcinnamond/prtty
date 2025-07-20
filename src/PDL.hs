module PDL (
  parseFiles,
  parseFile,
  fromHex,
  Expr (..),
  Presentation (..),
  TopLevelExpr (..),
  Arg(..),
  Args(..),
  Hex(..),
) where

import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, parse)
import PDL.Types (Presentation (..), Expr(..), TopLevelExpr(..), Hex(..), Arg(..), Args(..), fromHex)
import PDL.Internal (parsePresentation)

parseFiles :: [FilePath] -> IO (Either [ParseErrorBundle Text Void] Presentation)
parseFiles ps = do
  res <- mapM parseFile ps
  pure $ case lefts res of
    [] -> Right $ collapse $ rights res
    errs -> Left errs

collapse :: [Presentation] -> Presentation
collapse [] = Presentation []
collapse (p : ps) = foldl (<>) p ps

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Presentation)
parseFile p = do
  s <- TIO.readFile p
  pure $ parse parsePresentation p s
