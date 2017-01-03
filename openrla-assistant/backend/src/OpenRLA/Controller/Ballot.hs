module OpenRLA.Controller.Ballot where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.!=), object)
import           Data.Aeson.Types (Parser)
import           Data.Maybe (maybe)
import           Data.Text (unpack)
import           Network.HTTP.Types.Status (notFound404)
import           System.Directory (copyFile)
import           System.FilePath ((</>))
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Ballot as St
import           OpenRLA.Types (Ballot(..), State(..))


index :: Controller
index State { conn } = liftIO (St.index conn (0, 20)) >>= json

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
getById State { conn } = do
  balId <- param "id"
  res <- liftIO $ St.getById conn balId
  maybe (status notFound404) json res
