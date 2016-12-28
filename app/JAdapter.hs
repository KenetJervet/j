module JAdapter where

import           Control.Monad.Catch
import           J
import           System.IO.Error
import           System.IO.Unsafe
import           Util

defaultJStoreFile :: FilePath
defaultJStoreFile = unsafePerformIO $ expandHome "~/.j.conf"

load :: IO JStore
load = loadFile defaultJStoreFile
  `catch` \ex -> if isDoesNotExistError ex
                 then return empty
                 else error $ show ex

save :: JStore -> IO ()
save = saveFile defaultJStoreFile
