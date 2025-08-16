module Runtime.Internal.Exec where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Runtime.Internal.Types (Runtime)
import System.Posix.ByteString (executeFile, forkProcess, getProcessStatus)

exec :: Text -> Runtime
exec = liftIO . runCmd . TE.encodeUtf8

runCmd :: ByteString -> IO ()
runCmd cmd = do
    pid <- forkProcess $ do
        executeFile path True args Nothing
    _ <- getProcessStatus True False pid
    pure ()
  where
    path = case BS.words cmd of
        [] -> ""
        (x : _) -> x
    args = drop 1 (BS.words cmd)