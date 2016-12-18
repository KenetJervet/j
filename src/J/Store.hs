{-# LANGUAGE RecordWildCards #-}

module J.Store where

import Data.Csv
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector as V

newtype JStore = JStore { store :: HM.HashMap String String }

query :: String -> JStore -> Maybe String
query key JStore{..} = HM.lookup key store

update :: (String, String) -> JStore -> JStore
update (key, dst) JStore{..} = JStore $ HM.insert key dst store

delete :: String -> JStore -> JStore
delete key JStore{..} = JStore $ HM.delete key store

loadFile :: FilePath -> IO JStore
loadFile filePath = do
  csvData <- LBS.readFile filePath
  case decode NoHeader csvData of
    Left err -> error "Malformed CSV file"
    Right v -> return $ JStore (HM.fromList (V.toList v))

saveFile :: FilePath -> JStore -> IO ()
saveFile filePath JStore{..} = LBS.writeFile filePath $ encode $ HM.toList store
