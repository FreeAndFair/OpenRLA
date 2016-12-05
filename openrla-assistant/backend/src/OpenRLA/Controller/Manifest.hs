module OpenRLA.Controller.Manifest (create) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, Value, (.:), (.=), decode, object)
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust)
import           Data.Text (Text, unpack)
import           Database.SQLite.Simple (lastInsertRowId)
import           System.Directory (copyFile)
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement as St
import           OpenRLA.Types (State(..))
import qualified OpenRLA.Vendor.Dominion as Dominion


nameForManifest :: Text -> Text -> Integer -> FilePath
nameForManifest vendor mType mId = concat [ unpack vendor
                                          , "-"
                                          , unpack mType
                                          , "-"
                                          , show mId
                                          ]

create :: Controller
create state = parseThen createP createCb
  where
    createCb bodyData@(_, _, fileType, _) = do
      (newPath, newData) <- liftIO (createIO state bodyData)
      json $ object [ "filePath" .= newPath
                    , "type"     .= fileType
                    , "data"     .= newData
                    ]

createP :: Object -> Parser (Integer, Text, Text, Text)
createP o = do
  electionId <- o .: "electionId"
  vendor     <- o .: "vendor"
  fileType   <- o .: "type"
  srcPath    <- o .: "filePath"

  return (electionId, vendor, fileType, srcPath)

createIO :: State -> (Integer, Text, Text, Text) -> IO (FilePath, Value)
createIO state@State {..} (elId, vendor, fileType, srcPath) = do
  manifestId <- St.createManifest conn (vendor, fileType, srcPath)
  let newPath = dataRel $ nameForManifest vendor fileType manifestId
  copyFile (unpack srcPath) newPath
  St.setManifestPathForId conn manifestId newPath
  fileData <- BSL.readFile newPath
  let mObj = fromJust (decode fileData)
  newData <- case vendor of
    "dominion"    -> Dominion.processManifest state elId fileType mObj
    "freeandfair" -> undefined
    _             -> error "Invalid vendor"
  return (newPath, newData)
