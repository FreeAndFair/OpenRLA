module OpenRLA.Controller.Ballot where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:))
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


create :: Controller
create state = parseThen createP createCb
  where
    createCb (elId, srcPaths) = liftIO (createIO state elId srcPaths) >>= json

createP :: Object -> Parser (Integer, [FilePath])
createP o = do
  elId     <- o .: "electionId"
  srcPaths <- o .: "filePaths"
  return (elId, map unpack srcPaths)

createIO :: State -> Integer -> [FilePath] -> IO [Ballot]
createIO State { .. } elId srcPaths = do
  let relPath balId = dataDir </> "ballot" </> (show balId)
  ballots <- St.create conn elId srcPaths relPath
  let copyBallot (srcPath, ballot) = do
        let Ballot { balFilePath } = ballot
        copyFile srcPath balFilePath
  forM_ (zip srcPaths ballots) copyBallot
  return ballots

getById :: Controller
getById State { conn } = do
  balId <- param "id"
  res <- liftIO $ St.getById conn balId
  maybe (status notFound404) json res
