module OpenRLA.Controller.Ballot where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.!=), (.=), object)
import           Data.Aeson.Types (Parser)
import           Data.Maybe (maybe)
import           Data.Text (unpack)
import           System.Directory (copyFile)
import           System.FilePath ((</>))
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Ballot as St
import           OpenRLA.Types (Ballot(..), State(..))


index :: Controller
index State { conn } = parseThen indexP indexCb
  where
    indexCb args = liftIO (St.index conn args) >>= json

indexP :: Object -> Parser (Integer, Integer)
indexP o = do
  offset <- o .:? "offset" .!= 0
  limit  <- o .:? "limit"  .!= 20
  let boundedLimit = min limit 20
  return (boundedLimit, offset)

create :: Controller
create state = parseThen createP createCb
  where
    createCb srcPath = liftIO (createIO state srcPath) >>= json

createP :: Object -> Parser FilePath
createP o = do
  srcPath <- o .: "filePath"
  return $ unpack srcPath

createIO :: State -> FilePath -> IO Ballot
createIO State { .. } srcPath = do
  let mkPath balId = dataDir </> "ballot" </> (show balId)
  ballot <- St.create conn srcPath mkPath
  let Ballot { .. } = ballot
  copyFile srcPath balFilePath
  return ballot

getById :: Controller
getById State { conn } = parseThen (.: "ballotId") cb
  where
    cb balId = do
      res <- liftIO (St.getById conn balId)
      let pairs = maybe [] (\p -> [ "filePath" .= p ]) res
      json $ object pairs

setById :: Controller
setById = undefined
