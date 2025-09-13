module Runtime.Internal.IO where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Runtime.Internal.Types (Runtime)
import System.IO (hFlush, stdout)

-- | Send output to the terminal.
out :: Text -> Runtime
out t = liftIO $ TIO.putStr t >> hFlush stdout