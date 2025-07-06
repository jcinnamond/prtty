module Main where

import Runtime (Duration (..), Instruction (..), runPresentation)
import VT

main :: IO ()
main =
    runPresentation
        [ Output $ bgColor "214" <> fgColor "21" <> bold <> "hi" <> reset
        , WaitForInput
        , Exec "hx cabal.project"
        , Output $ bgRGB 0 0 0xff <> fgRGB 0x99 0x99 0 <> "some text" <> reset
        , Pause (Second 3)
        , Output $ MoveTo 15 10 <> "was it worth the wait?"
        , WaitForInput
        , End
        ]
