{- |
Description: Primitive functions for the runtime.

These are kept separate to avoid circular imports.
-}
module Runtime.Internal.Primitive where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Runtime.Internal.Types (Runtime, Runtime')
import Runtime.Value (Duration, nanoseconds)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import VT qualified

-- | Send output to the terminal.
out :: Text -> Runtime
out = liftIO . outIO

outIO :: Text -> IO ()
outIO t = TIO.putStr t >> hFlush stdout

{- | Block until a keypress is received.
This also handles pageup, which is a multi-character input in a terminal.
-}
waitForKeypress :: Runtime' Keypress
waitForKeypress = do
    c <- liftIO getChar
    case c of
        '\ESC' -> do
            code <- liftIO $ getChars 3
            if code == "[5~"
                then pure PageUp
                else pure Unrecognised
        x -> pure $ Key x

data Keypress
    = Key Char
    | PageUp
    | Unrecognised

-- | Read a specified number of characters from stdin.
getChars :: Int -> IO [Char]
getChars = sequence . flip replicate getChar

-- | Read a whole line of input, echoing the input
getLine :: IO Text
getLine = do
    T.pack <$> withEcho Prelude.getLine

{- | Temporarily enable echoing of characters while running a single function.
This is useful when prompting for input.
-}
withEcho :: IO a -> IO a
withEcho f = do
    hSetEcho stdin True
    x <- f
    hSetEcho stdin False
    pure x

-- | Explict no-op
nochange :: Runtime
nochange = pure ()

-- | Temporarily block execution.
pause :: Duration -> Runtime
pause d = liftIO $ threadDelay $ nanoseconds d

-- | Prepare a terminal for running a presentation.
initializeTerminal :: IO ()
initializeTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    resetIO

-- | Restore the terminal once the presentation has finished.
restoreTerminal :: IO ()
restoreTerminal = do
    resetIO
    outIO $ VT.showCursor <> VT.noAltBuffer

-- | Restore terminal defaults
reset :: Runtime
reset = liftIO resetIO

resetIO :: IO ()
resetIO = outIO $ VT.altBuffer <> VT.clear <> VT.moveTo 0 0 <> VT.hideCursor