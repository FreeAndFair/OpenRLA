module Controller.Manifest (create) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:), (.=), decode, object)
import           Data.Aeson.Types (Parser)
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


create :: Controller
create State { .. } = parseThen p cb
  where
    p o = do
      vendor   <- o .: "vendor"
      fileType <- o .: "type"
      srcPath  <- o .: "filePath"
      return (vendor, fileType, srcPath) :: Parser (Text, Text, Text)

    cb index = do
      let (vendor, fileType, srcPath) = index
      newPath <- liftIO $ do
        Q.createManifest conn index
        rowId <- lastInsertRowId conn
        let manifestId = fromIntegral rowId
        let filePath = dataRel (show manifestId)
        copyFile (unpack srcPath) filePath
        Q.setManifestPathForId conn manifestId filePath
        fileData <- BSL.readFile filePath
        let mObj = fromJust (decode fileData)
        case vendor of
          "dominion"    -> Vendor.Dominion.processManifest fileType mObj
          "freeandfair" -> undefined
          _             -> error "Invalid vendor"
        return filePath
      json $ object [ "filePath" .= newPath
                    , "type"     .= fileType
                    , "data"     .= object []
                    ]
