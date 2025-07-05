module Cmd (run)
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import System.Posix.ByteString (executeFile, forkProcess, getProcessStatus)

run :: ByteString -> IO ()
run cmd = do
    pid <- forkProcess $ do
        executeFile path True args Nothing
    _ <- getProcessStatus True False pid
    pure ()
  where
    path = case BS.words cmd of
        [] -> ""
        (x : _) -> x
    args = drop 1 (BS.words cmd)