module Main where

import VT
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    TIO.putStrLn . toText $
        bgColor "214" <> fgColor "21" <> bold <> "hi" <> Reset

    TIO.putStrLn . toText $ 
        bgRGB 0 0 0xff <> fgRGB 0x99 0x99 0 <> "some text" <> Reset
