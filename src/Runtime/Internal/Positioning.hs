module Runtime.Internal.Positioning where

import Control.Monad.State (gets)
import Runtime.Internal.Output (out)
import Runtime.Internal.Types (Environment (..), Runtime, Runtime')
import VT qualified

data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving stock (Show, Eq)

xFromCenter :: Int -> Runtime' Int
xFromCenter x = do
    w <- gets width
    pure $ w `div` 2 - x `div` 2

yFromCenter :: Int -> Runtime' Int
yFromCenter y = do
    h <- gets height
    pure $ h `div` 2 - y `div` 2

posFromHome :: Runtime' Position
posFromHome = do
    x <- gets leftMargin
    y <- gets topMargin
    pure $ Position{x, y}

runHome :: Runtime
runHome = do
    y <- gets topMargin
    x <- gets leftMargin
    out $ VT.moveTo y x

runCenter :: Int -> Runtime
runCenter x = do
    width <- gets width
    let col = width `div` 2 - x `div` 2
    out $ VT.moveToCol col

runVCenter :: Int -> Runtime
runVCenter x = do
    height <- gets height
    left <- gets leftMargin
    let row = height `div` 2 - x `div` 2
    out $ VT.moveTo row left
    nochange

nochange :: Runtime
nochange = pure ()
