module Main where

import Compiler.Compiler (compile, resolveReferences, rewrite)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Options (Options (..), options)
import Options.Applicative (ParserInfo, execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser.AST (Presentation)
import Parser.AST qualified as AST
import Parser.Parser (parseFiles)
import PrettyPrint (Empty (..), PrettyPrint (..))
import Runtime.Instructions (Instruction)
import Runtime.Run qualified as Runtime

type PrttyError = Text
type App a = ExceptT PrttyError IO a

main :: IO ()
main = do
    o <- execParser opts

    result <-
        runExceptT $
            runParser o >>= runResolver o >>= runRewriter o >>= runCompiler o >>= runPresentation o
    case result of
        Left err -> TIO.putStrLn err
        Right _ -> pure ()

opts :: ParserInfo Options
opts =
    info
        (options <**> helper)
        (fullDesc <> progDesc "presentation software in your terminal")

runParser :: Options -> App [Presentation]
runParser o = do
    p <- liftIO $ parseFiles o.inputs
    handlingError p $ if o.debugAST then debug else pure

runResolver :: Options -> [Presentation] -> App [AST.Expr]
runResolver o p = do
    let es = resolveReferences p
    handlingError es $ if o.debugResolver then debug else pure

runRewriter :: Options -> [AST.Expr] -> App [AST.Expr]
runRewriter o p = do
    let p' = rewrite p
    handlingError p' $ if o.debugRewrite then debug else pure

runCompiler :: Options -> [AST.Expr] -> App (Vector Instruction)
runCompiler o p = do
    let ir = compile o p
    handlingError ir $ if o.debugIR then debug else pure

runPresentation :: Options -> Vector Instruction -> App ()
runPresentation _ ir = liftIO $ Runtime.run ir

debug :: (PrettyPrint a, Empty a) => a -> App a
debug x = do
    liftIO $ TIO.putStrLn $ pretty x
    pure empty

handlingError :: Either Text b -> (b -> App b) -> App b
handlingError (Left err) _ = throwError err
handlingError (Right x) f = f x