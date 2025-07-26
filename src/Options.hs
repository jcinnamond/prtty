module Options (
    Options (..),
    options,
) where

import Options.Applicative (Parser, argument, help, long, metavar, some, str, switch)

data Options = Options
    { debugAST :: !Bool
    , debugIR :: !Bool
    , inputs :: [FilePath]
    }
    deriving stock (Show, Eq)

options :: Parser Options
options =
    Options
        <$> switch
            ( long "debugAST"
                <> help "Print out the AST"
            )
        <*> switch
            ( long "debugIR"
                <> help "Print out the runtime instructions"
            )
        <*> some (argument str (metavar "[source file, ...]"))