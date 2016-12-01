module Controller.Manifest (create) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:), (.=), decode, object)
import           Data.Aeson.Types (Object, Parser)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust)
import           Data.Text (Text, unpack)
import           Database.SQLite.Simple (lastInsertRowId)
import           System.Directory (copyFile)
import           Web.Scotty (json)

import           Controller
import qualified Query as Q
import           Types (State(..))
import qualified Vendor.Dominion


nameForManifest :: Text -> Text -> Integer -> FilePath
nameForManifest vendor mType mId = concat [ unpack vendor
                                          , "-"
                                          , unpack mType
                                          , "-"
                                          , show mId
                                          ]

create :: Controller
create state@State { .. } = parseThen p cb
  where
    p :: Object -> Parser (Integer, Text, Text, Text)
    p o = do
      electionId <- o .: "electionId"
      vendor     <- o .: "vendor"
      fileType   <- o .: "type"
      srcPath    <- o .: "filePath"
      return (electionId, vendor, fileType, srcPath)

    cb (electionId, vendor, fileType, srcPath) = do
      (newPath, newData) <- liftIO $ do
        Q.createManifest conn (vendor, fileType, srcPath)
        rowId <- lastInsertRowId conn
        let manifestId = fromIntegral rowId
        let newPath = dataRel $ nameForManifest vendor fileType manifestId
        copyFile (unpack srcPath) newPath
        Q.setManifestPathForId conn manifestId newPath
        fileData <- BSL.readFile newPath
        let mObj = fromJust (decode fileData)
        newData <- case vendor of
          "dominion"    -> Vendor.Dominion.processManifest state electionId fileType mObj
          "freeandfair" -> undefined
          _             -> error "Invalid vendor"
        return (newPath, newData)
      json $ object [ "filePath" .= newPath
                    , "type"     .= fileType
                    , "data"     .= newData
                    ]
