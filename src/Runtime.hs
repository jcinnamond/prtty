module Runtime (
    runPresentation,
    Instruction (..),
    Duration (..),
) where

import Cmd qualified
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (ByteString)
import Data.Text.IO qualified as TIO
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import VT (Displayable (AltBuffer, NoAltBuffer), toText)

data Instruction
    = Output Displayable
    | Exec ByteString
    | Pause Duration
    | WaitForInput
    | End
    deriving stock (Show, Eq)

data Duration = Second Int | Millisecond Int
    deriving stock (Show, Eq)

runPresentation :: [Instruction] -> IO ()
runPresentation is = initTerminal >> mapM_ runInstruction is

initTerminal :: IO ()
initTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    output AltBuffer

runInstruction :: Instruction -> IO ()
runInstruction (Output t) = output t >> hFlush stdout
runInstruction (Exec c) = Cmd.run c >> output AltBuffer
runInstruction (Pause d) = pause d
runInstruction WaitForInput = waitForInput
runInstruction End = output NoAltBuffer

output :: Displayable -> IO ()
output = TIO.putStrLn . toText

waitForInput :: IO ()
waitForInput = do
    _ <- getChar
    pure () -- TODO: handle `b` to go back in a (currently non-existent) callstack

pause :: Duration -> IO ()
pause (Second x) = threadDelay (x * 1000000)
pause (Millisecond x) = threadDelay (x * 1000)