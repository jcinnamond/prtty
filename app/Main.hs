module Main where

import Cmd (run)
import Data.Text.IO qualified as TIO
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import VT

out :: Displayable -> IO ()
out x = TIO.putStr (toText x) >> hFlush stdout

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    out $ AltBuffer <> Home

    out $
        bgColor "214" <> fgColor "21" <> bold <> "hi" <> reset

    _ <- getChar

    run "hx cabal.project"

    out $ AltBuffer <> MoveTo 5 10

    out $ bgRGB 0 0 0xff <> fgRGB 0x99 0x99 0 <> "some text" <> reset

    _ <- getChar

    out NoAltBuffer
