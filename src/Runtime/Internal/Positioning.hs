module Runtime.Internal.Positioning where

import Control.Monad.State (gets)
import Runtime.Internal.Output (out)
import Runtime.Internal.Types (Anchor (..), Environment (..), Runtime)
import Runtime.Value
import Runtime.Value qualified as RuntimeValue
import VT qualified

data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving stock (Show, Eq)

runHome :: Runtime
runHome = do
    y <- gets topMargin
    x <- gets leftMargin
    out $ VT.moveTo y x

runMoveTo :: Maybe Value -> Maybe Value -> Anchor -> Runtime
runMoveTo (Just y) Nothing anchor = moveToY y anchor
runMoveTo Nothing (Just x) anchor = moveToX x anchor
runMoveTo (Just y) (Just x) anchor = moveToYX y x anchor
runMoveTo Nothing Nothing _ = pure ()

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

resolve :: Value -> Anchor -> Int -> Int -> Int
resolve (RuntimeValue.Number x) TopLeft _ _ = x
resolve (RuntimeValue.Rational n d) TopLeft limit _ = limit * n `div` d
resolve (RuntimeValue.Percentage x) TopLeft limit margin = resolve (RuntimeValue.Rational x 100) TopLeft limit margin
resolve v Margin limit margin = resolve v TopLeft limit margin + margin
resolve v BottomRight limit margin = limit - resolve v TopLeft limit margin
resolve v _ _ _ = error $ "can't move to " <> show v

moveToY :: Value -> Anchor -> Runtime
moveToY y anchor = do
    height <- gets height
    topMargin <- gets topMargin
    leftMargin <- gets leftMargin
    out $ VT.moveToRow (resolve y anchor height topMargin) <> VT.moveToCol leftMargin

moveToX :: Value -> Anchor -> Runtime
moveToX x anchor = do
    width <- gets width
    leftMargin <- gets leftMargin
    out $ VT.moveToCol (resolve x anchor width leftMargin)

moveToYX :: Value -> Value -> Anchor -> Runtime
moveToYX y x anchor = do
    height <- gets height
    topMargin <- gets topMargin
    width <- gets width
    leftMargin <- gets leftMargin
    out $ VT.moveTo (resolve y anchor height topMargin) (resolve x anchor width leftMargin)
