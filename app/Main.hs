module Main where

import Data.Either (partitionEithers)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PDL (parseFile)
import Reduce (reduce)
import Runtime (runPresentation)
import System.Environment (getArgs)

main :: IO ()
main = do
    parseResult <- getArgs >>= mapM parseFile
    case partitionEithers parseResult of
        ([], exprs) -> do
            case reduce (concat exprs) of
                Left err -> TIO.putStrLn $ T.pack $ show err
                Right instructions -> runPresentation instructions
        (es, _) -> TIO.putStrLn $ T.pack $ show es
