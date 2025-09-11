module Runtime.Internal.Run where

import Control.Monad.State (execStateT)
import Data.Vector ((!?))
import Runtime.Internal.Navigation (jump)
import Runtime.Internal.Positioning (runCenter, runHome, runMoveTo, setLeftMargin, setTopMargin)
import Runtime.Internal.Types (Environment (..), Instruction (..), Runtime)

run' :: Environment -> IO ()
run' e = do
    case e.instructions !? e.pc of
        Nothing -> pure ()
        (Just i) -> do
            e' <- execStateT (runInstruction i) e{pc = e.pc + 1}
            run' e'

runInstruction :: Instruction -> Runtime
runInstruction (JumpTo i) = jump i
runInstruction (SetMarker _) = pure ()
runInstruction Home = runHome
runInstruction (MoveTo y x anchor) = runMoveTo y x anchor
runInstruction (Center x) = runCenter x
runInstruction (VCenter x) = runCenter x
runInstruction (SetTopMargin x) = setTopMargin x -- modify $ setTopMargin x
runInstruction (SetLeftMargin x) = setLeftMargin x -- modify $ setLeftMargin x
runInstruction _ = undefined

-- runInstruction (Output t) = out t
-- runInstruction Newline = out "\n" >> moveToLeftMargin
-- runInstruction StoreBackMarker = modify storeBackMarker
-- runInstruction (VSpace x) = out (VT.moveDown x) >> moveToLeftMargin
-- runInstruction WaitForInput = runWaitForInput
-- runInstruction (Pause d) = liftIO $ threadDelay $ nanoseconds d
-- runInstruction (SetStyle style) = runSetStyle style
-- runInstruction SaveStyle = modify storeCurrentStyle
-- runInstruction RestoreStyle = runRestoreStyles
-- runInstruction (Exec cmd) = liftIO $ Cmd.run $ TE.encodeUtf8 cmd
-- runInstruction Reset = runReset
