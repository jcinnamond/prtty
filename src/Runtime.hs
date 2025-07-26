module Runtime (
    runPresentation,
    Instruction (..),
    Duration (..),
) where

import Cmd qualified
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Data.ByteString.Char8 (ByteString)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PrettyPrint (PrettyPrint (..))
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import VT (Displayable (AltBuffer, ClearScreen, HideCursor, Home, NoAltBuffer, ShowCursor), toText)
import VT qualified

data Instruction
    = Output Displayable
    | Exec ByteString
    | Pause Duration
    | WaitForInput
    | Center !Int
    | VCenter !Int
    | MoveTo !Int !Int
    | End
    deriving stock (Show, Eq)

instance PrettyPrint Instruction where
    pretty i = T.show i <> "\n"

data Duration = Second Int | Millisecond Int
    deriving stock (Show, Eq)

type Runtime = ReaderT Env IO

data Env = Env
    { height, width :: !Int
    }
    deriving stock (Show, Eq)

runPresentation :: [Instruction] -> IO ()
runPresentation is = do
    env <- getEnv
    initTerminal >> mapM_ (\i -> runReaderT (runInstruction i) env) is >> restoreTerminal

getEnv :: IO Env
getEnv = do
    size <- TSize.size
    pure $ case size of
        Just (TSize.Window{height, width}) -> Env height width
        Nothing -> defaultEnv

defaultEnv :: Env
defaultEnv = Env 80 25

initTerminal :: IO ()
initTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    output AltBuffer
    output ClearScreen
    output Home
    output HideCursor

restoreTerminal :: IO ()
restoreTerminal = do
    output ShowCursor
    output NoAltBuffer

runInstruction :: Instruction -> Runtime ()
runInstruction (Output t) = liftIO $ output t >> hFlush stdout
runInstruction (Exec c) = liftIO $ Cmd.run c >> output AltBuffer
runInstruction (Pause d) = liftIO $ pause d
runInstruction WaitForInput = liftIO waitForInput
runInstruction End = liftIO $ output NoAltBuffer
runInstruction (MoveTo x y) = liftIO $ output (VT.MoveTo y x)
runInstruction (Center l) = do
    w <- asks width
    liftIO $ output (VT.MoveToCol (w `div` 2 - l `div` 2))
runInstruction (VCenter x) = do
    h <- asks height
    liftIO $ output (VT.MoveTo (h `div` 2 - x `div` 2) 0)

output :: Displayable -> IO ()
output = TIO.putStr . toText

waitForInput :: IO ()
waitForInput = do
    _ <- getChar
    pure () -- TODO: handle `b` to go back in a (currently non-existent) callstack

pause :: Duration -> IO ()
pause (Second x) = threadDelay (x * 1000000)
pause (Millisecond x) = threadDelay (x * 1000)