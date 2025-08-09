module Runtime.Run (run) where

import Cmd qualified
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Runtime.Instructions (Anchor (..), Instruction (..), Style (..))
import Runtime.Value (Value (..), nanoseconds)
import Runtime.Value qualified as RuntimeValue
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import VT qualified

type Runtime = StateT Environment IO ()

data Environment = Environment
    { instructions :: Vector Instruction
    , pc :: Int
    -- ^ "program counter", a pointer to the next instruction to execute
    , backMarkers :: [Int]
    -- ^ a list of points to jump back to
    , lastJump :: Maybe Int
    -- ^ the last instruction we jumped back to, used to support pressing 'b' multiple times
    , height :: Int
    -- ^ the height of the terminal, in characters
    , width :: Int
    -- ^ the width of the terminal, in characters
    , topMargin :: Int
    , leftMargin :: Int
    , currentStyle :: Style
    , styleHistory :: [Style]
    }
    deriving stock (Show, Eq)

newEnvironment :: Vector Instruction -> IO Environment
newEnvironment is = do
    size <- TSize.size
    let (theight, twidth) = case size of
            Just (TSize.Window{height, width}) -> (height, width)
            Nothing -> (80, 25)
    pure $
        Environment
            { instructions = is
            , pc = 0
            , backMarkers = []
            , lastJump = Nothing
            , height = theight
            , width = twidth
            , topMargin = 0
            , leftMargin = 0
            , currentStyle =
                Style
                    { fgColor = Just $ RGB 0 0 0
                    , bgColor = Just $ RGB 255 255 255
                    , bold = Nothing
                    }
            , styleHistory = []
            }

run :: Vector Instruction -> IO ()
run is = do
    initializeTerminal
    env <- newEnvironment is
    run' env
    restoreTerminal

initializeTerminal :: IO ()
initializeTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    reset

reset :: IO ()
reset =
    out $
        VT.altBuffer
            <> VT.clear
            <> VT.moveTo 0 0
            <> VT.hideCursor

restoreTerminal :: IO ()
restoreTerminal = do
    reset
    out $
        VT.showCursor
            <> VT.noAltBuffer

run' :: Environment -> IO ()
run' e = do
    case e.instructions !? e.pc of
        Nothing -> pure ()
        (Just i) -> do
            e' <- execStateT (runInstruction i) e{pc = e.pc + 1}
            run' e'

runInstruction :: Instruction -> Runtime
runInstruction (SetMarker _) = pure ()
runInstruction (JumpTo i) = runJump i
runInstruction (Output t) = out t
runInstruction Newline = out "\n" >> moveToLeftMargin
runInstruction StoreBackMarker = modify storeBackMarker
runInstruction (SetTopMargin x) = modify $ setTopMargin x
runInstruction (SetLeftMargin x) = modify $ setLeftMargin x
runInstruction Home = runHome
runInstruction (MoveTo y x anchor) = runMoveTo y x anchor
runInstruction (Center x) = runCenter x
runInstruction (VCenter x) = runVCenter x
runInstruction (VSpace x) = out (VT.moveDown x) >> moveToLeftMargin
runInstruction WaitForInput = runWaitForInput
runInstruction (Pause d) = liftIO $ threadDelay $ nanoseconds d
runInstruction (SetStyle style) = runSetStyle style
runInstruction SaveStyle = modify storeCurrentStyle
runInstruction RestoreStyle = runRestoreStyles
runInstruction (Exec cmd) = liftIO $ Cmd.run $ TE.encodeUtf8 cmd
runInstruction Reset = runReset

runJump :: Int -> Runtime
runJump i = modify (\e -> e{pc = i})

runMoveTo :: Maybe Value -> Maybe Value -> Anchor -> Runtime
runMoveTo (Just y) Nothing anchor = moveToY y anchor
runMoveTo Nothing (Just x) anchor = moveToX x anchor
runMoveTo (Just y) (Just x) anchor = moveToYX y x anchor
runMoveTo Nothing Nothing _ = pure ()

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

resolve :: Value -> Anchor -> Int -> Int -> Int
resolve (RuntimeValue.Number x) TopLeft _ _ = x
resolve (RuntimeValue.Rational n d) TopLeft limit _ = limit * n `div` d
resolve (RuntimeValue.Percentage x) TopLeft limit margin = resolve (RuntimeValue.Rational x 100) TopLeft limit margin
resolve v Margin limit margin = resolve v TopLeft limit margin + margin
resolve v BottomRight limit margin = limit - resolve v TopLeft limit margin
resolve v _ _ _ = error $ "can't move to " <> show v

runReset :: Runtime
runReset = do
    liftIO reset
    outputStyle =<< gets currentStyle

moveToLeftMargin :: Runtime
moveToLeftMargin = do
    margin <- gets leftMargin
    when (margin > 1) $ do
        out $ VT.moveToCol 0
        out $ VT.moveRight $ margin - 1

storeCurrentStyle :: Environment -> Environment
storeCurrentStyle e = e{styleHistory = e.currentStyle : e.styleHistory}

runRestoreStyles :: Runtime
runRestoreStyles = do
    h <- gets styleHistory
    case h of
        [] -> nochange
        (x : xs) -> do
            outputStyle x
            modify (\e -> e{currentStyle = x, styleHistory = xs})

outputStyle :: Style -> Runtime
outputStyle s = out $ styleFgColor s.fgColor <> styleBgColor s.bgColor <> styleBold s.bold
  where
    styleFgColor Nothing = ""
    styleFgColor (Just (RGB r g b)) = VT.fgColor r g b
    styleFgColor _ = error "unimplemented"

    styleBgColor Nothing = ""
    styleBgColor (Just (RGB r g b)) = VT.bgColor r g b
    styleBgColor _ = error "unimplemented"

    styleBold Nothing = VT.resetBold
    styleBold (Just Toggle) = VT.bold
    styleBold _ = error "unimplemented"

runSetStyle :: Style -> Runtime
runSetStyle s = do
    outputStyle s
    modify (\e -> e{currentStyle = e.currentStyle <> s})

runWaitForInput :: Runtime
runWaitForInput = do
    c <- liftIO getChar
    case c of
        '\ESC' -> do
            c' <- liftIO getChar
            c'' <- liftIO getChar
            c''' <- liftIO getChar
            if [c', c'', c''']
                == "[5~"
                then runMoveBack
                else nochange
        'b' -> runMoveBack
        'q' -> modify stop
        '.' -> modify stop
        _ -> nochange

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

stop :: Environment -> Environment
stop e = e{pc = V.length e.instructions}

runMoveBack :: Runtime
runMoveBack = do
    markers <- gets backMarkers
    lastJump <- gets lastJump
    modify $ moveBack lastJump markers

moveBack :: Maybe Int -> [Int] -> Environment -> Environment
moveBack lj bm e =
    case bm of
        [] -> e
        [x] ->
            -- if this is the last jump marker then don't lose it
            e{pc = x}
        (x : xs) ->
            if Just x == lj
                then moveBack Nothing xs e{backMarkers = xs, lastJump = Nothing}
                else e{pc = x, lastJump = Just x}

storeBackMarker :: Environment -> Environment
storeBackMarker e = e{backMarkers = e.pc : e.backMarkers}

setTopMargin :: Value -> Environment -> Environment
setTopMargin x e = e{topMargin = fromNumerical x e.height}

setLeftMargin :: Value -> Environment -> Environment
setLeftMargin x e = e{leftMargin = fromNumerical x e.width}

fromNumerical :: Value -> Int -> Int
fromNumerical (Number x) _ = x
fromNumerical (Rational n d) x = x * n `div` d
fromNumerical (Percentage p) x = x * 100 `div` p
fromNumerical _ _ = error "non-numerical argument"

out :: (MonadIO m) => Text -> m ()
out t = liftIO $ TIO.putStr t >> hFlush stdout