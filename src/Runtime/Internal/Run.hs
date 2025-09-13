module Runtime.Internal.Run where

import Control.Monad.State (execStateT)
import Data.Vector ((!?))
import Runtime.Internal.IO (out)
import Runtime.Internal.Navigation qualified as Navigation
import Runtime.Internal.Output qualified as Output
import Runtime.Internal.Positioning qualified as Positioning
import Runtime.Internal.Types (Environment (..), Instruction (..), Runtime)

run' :: Environment -> IO ()
run' e = do
    case e.instructions !? e.pc of
        Nothing -> pure ()
        (Just i) -> do
            e' <- execStateT (runInstruction i) e{pc = e.pc + 1}
            run' e'

runInstruction :: Instruction -> Runtime
runInstruction (SetMarker _) = pure ()
runInstruction (JumpTo i) = Navigation.jump i
runInstruction StoreBackMarker = Navigation.storeBackMarker
runInstruction Home = Positioning.home
runInstruction (MoveTo y x anchor) = Positioning.moveTo y x anchor
runInstruction (Center x) = Positioning.center x
runInstruction (VCenter x) = Positioning.vCenter x
runInstruction (SetTopMargin x) = Positioning.setTopMargin x
runInstruction (SetLeftMargin x) = Positioning.setLeftMargin x
runInstruction (Output t) = out t
runInstruction Newline = Output.newline
runInstruction (VSpace x) = Output.vspace x
runInstruction _ = undefined

-- runInstruction WaitForInput = runWaitForInput
-- runInstruction (Pause d) = liftIO $ threadDelay $ nanoseconds d
-- runInstruction (SetStyle style) = runSetStyle style
-- runInstruction SaveStyle = modify storeCurrentStyle
-- runInstruction RestoreStyle = runRestoreStyles
-- runInstruction (Exec cmd) = liftIO $ Cmd.run $ TE.encodeUtf8 cmd
-- runInstruction Reset = runReset
