module Runtime.Run (run) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Text.IO qualified as TIO
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Runtime.Instructions (Instruction (..), Numerical (..), RGB (..))
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
    , y :: Int
    -- ^ current cursor column, used for moving row
    , x :: Int
    -- ^ current cursor row, used for moving column (e.g., newline)
    , topMargin :: Int
    , leftMargin :: Int
    , style :: Style
    }
    deriving stock (Show, Eq)

data Style = Style
    { fgColor :: RGB
    , bgColor :: RGB
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
            , y = 0
            , x = 0
            , style =
                Style
                    { fgColor = RGB 255 255 255
                    , bgColor = RGB 0 0 0
                    }
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
    TIO.putStr $
        VT.altBuffer
            <> VT.clear
            <> VT.moveTo 0 0
            <> VT.hideCursor
    hFlush stdout

restoreTerminal :: IO ()
restoreTerminal = do
    TIO.putStr $
        VT.showCursor
            <> VT.noAltBuffer
    hFlush stdout

run' :: Environment -> IO ()
run' e = do
    case e.instructions !? e.pc of
        Nothing -> pure ()
        (Just i) -> do
            e' <- execStateT (runInstruction i) e{pc = e.pc + 1}
            run' e'

runInstruction :: Instruction -> Runtime
runInstruction (Output t) = liftIO $ TIO.putStr t >> hFlush stdout
runInstruction StoreBackMarker = modify storeBackMarker
runInstruction (SetTopMargin x) = modify $ setTopMargin x
runInstruction (SetLeftMargin x) = modify $ setLeftMargin x
runInstruction Home = do
    y <- gets topMargin
    x <- gets leftMargin
    liftIO $ TIO.putStr $ VT.moveTo y x
    modify home
runInstruction WaitForInput = do
    c <- liftIO getChar
    case c of
        'b' -> runMoveBack
        'q' -> modify stop
        _ -> modify id

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

home :: Environment -> Environment
home e = e{x = e.leftMargin, y = e.topMargin}

storeBackMarker :: Environment -> Environment
storeBackMarker e = e{backMarkers = e.pc : e.backMarkers}

setTopMargin :: Numerical -> Environment -> Environment
setTopMargin x e = e{topMargin = fromNumerical x e.height}

setLeftMargin :: Numerical -> Environment -> Environment
setLeftMargin x e = e{leftMargin = fromNumerical x e.width}

fromNumerical :: Numerical -> Int -> Int
fromNumerical (Number x) _ = x
fromNumerical (Rational n d) x = x * n `div` d
fromNumerical (Percent p) x = x * 100 `div` p
