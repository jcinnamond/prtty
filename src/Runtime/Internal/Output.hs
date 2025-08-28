module Runtime.Internal.Output where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.IO (hFlush, stdout)

out :: (MonadIO m) => Text -> m ()
out t = liftIO $ TIO.putStr t >> hFlush stdout