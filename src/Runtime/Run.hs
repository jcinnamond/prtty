module Runtime.Run (run) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Runtime.Instructions (Instruction (..), Style (..))
import Runtime.Value (Value (..), nanoseconds)
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
    out $
        VT.altBuffer
            <> VT.clear
            <> VT.moveTo 0 0
            <> VT.hideCursor

restoreTerminal :: IO ()
restoreTerminal = do
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
runInstruction (Output t) = out t
runInstruction Newline = runNewline
runInstruction StoreBackMarker = modify storeBackMarker
runInstruction (SetTopMargin x) = modify $ setTopMargin x
runInstruction (SetLeftMargin x) = modify $ setLeftMargin x
runInstruction (Center x) = runCenter x
runInstruction (VCenter x) = runVCenter x
runInstruction Home = runHome
runInstruction WaitForInput = runWaitForInput
runInstruction (Pause d) = liftIO $ threadDelay $ nanoseconds d
runInstruction (SetStyle style) = runSetStyle style
runInstruction SaveStyle = modify storeCurrentStyle
runInstruction RestoreStyle = runRestoreStyles

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
        'b' -> runMoveBack
        'q' -> modify stop
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

runNewline :: Runtime
runNewline = do
    margin <- gets leftMargin
    out "\n"
    when (margin > 1) $ out $ VT.moveRight $ margin - 1

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