module Runtime.Internal.Run where

import Control.Monad.State (execStateT)
import Data.Vector ((!?))
import Runtime.Internal.Exec qualified as Exec
import Runtime.Internal.IO qualified as IO
import Runtime.Internal.Navigation qualified as Navigation
import Runtime.Internal.Positioning qualified as Positioning
import Runtime.Internal.Primitive qualified as Primitive
import Runtime.Internal.Style qualified as Style
import Runtime.Internal.Types (Environment (..), Instruction (..), Runtime)

run :: Environment -> IO ()
run e = do
    case e.instructions !? e.pc of
        Nothing -> pure ()
        (Just i) -> do
            e' <- execStateT (runInstruction i) e{pc = e.pc + 1}
            run e'

runInstruction :: Instruction -> Runtime
runInstruction (SetMarker _) = Primitive.nochange
runInstruction (JumpTo i) = Navigation.jump i
runInstruction StoreBackMarker = Navigation.storeBackMarker
runInstruction Home = Positioning.home
runInstruction (MoveTo y x anchor) = Positioning.moveTo y x anchor
runInstruction (Center x) = Positioning.center x
runInstruction (VCenter x) = Positioning.vCenter x
runInstruction (SetTopMargin x) = Positioning.setTopMargin x
runInstruction (SetLeftMargin x) = Positioning.setLeftMargin x
runInstruction (Output t) = Primitive.out t
runInstruction Newline = IO.newline
runInstruction (VSpace x) = IO.vspace x
runInstruction WaitForInput = IO.waitForInput
runInstruction (Pause d) = Primitive.pause d
runInstruction (SetStyle s) = Style.set s
runInstruction SaveStyle = Style.save
runInstruction RestoreStyle = Style.restore
runInstruction (Exec cmd) = Exec.exec cmd
runInstruction Reset = Primitive.reset >> Style.reapply
