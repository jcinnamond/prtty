module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PDL (parseFiles)
import Reduce (reduce)
import Runtime (runPresentation)
import System.Environment (getArgs)

main :: IO ()
main = do
    parseResult <- getArgs >>= parseFiles
    case parseResult of
        Left errs -> TIO.putStrLn $ T.pack $ show errs
        Right presentation ->
            case reduce presentation of
                Left err -> TIO.putStrLn $ T.pack $ show err
                Right instructions -> runPresentation instructions
