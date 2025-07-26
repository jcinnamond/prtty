module Main where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options (Options (..), options)
import Options.Applicative (ParserInfo, execParser, fullDesc, helper, info, progDesc, (<**>))
import PDL (Presentation (..), parseFiles)
import PrettyPrint (Empty (..), PrettyPrint (..))
import Reduce (reduce)
import Runtime (Instruction)
import Runtime qualified

type PrttyError = Text
type App a = ExceptT PrttyError IO a

main :: IO ()
main = do
    o <- execParser opts

    result <-
        runExceptT $
            runParser o >>= runCompiler o >>= runPresentation o
    case result of
        Left err -> TIO.putStrLn err
        Right _ -> pure ()

opts :: ParserInfo Options
opts =
    info
        (options <**> helper)
        (fullDesc <> progDesc "presentation software in your terminal")

runParser :: Options -> App Presentation
runParser o = do
    p <- liftIO $ parseFiles o.inputs
    handlingError p $ if o.debugAST then debug else pure

runCompiler :: Options -> Presentation -> App [Instruction]
runCompiler o p = do
    let ir = reduce p
    handlingError ir $ if o.debugIR then debug else pure

runPresentation :: Options -> [Instruction] -> App ()
runPresentation _ ir = liftIO $ Runtime.runPresentation ir

debug :: (PrettyPrint a, Empty a) => a -> App a
debug x = do
    liftIO $ TIO.putStrLn $ pretty x
    pure empty

handlingError :: (Show e) => Either e b -> (b -> App b) -> App b
handlingError (Left err) _ = throwError $ T.show err
handlingError (Right x) f = f x