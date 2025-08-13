module Options (
    Options (..),
    options,
) where

import Options.Applicative (Parser, argument, help, long, metavar, optional, some, str, strOption, switch)

data Options = Options
    { debugAST :: !Bool
    , debugIR :: !Bool
    , startAt :: !(Maybe String)
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
        <*> optional
            ( strOption
                ( long "startAt"
                    <> metavar "MARKER"
                    <> help "Start the presentation from a given marker"
                )
            )
        <*> some (argument str (metavar "[source file, ...]"))