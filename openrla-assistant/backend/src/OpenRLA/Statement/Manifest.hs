module OpenRLA.Statement.Manifest where

import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection)


createManifest :: Connection -> (Text, Text, Text) -> IO (Integer)
createManifest conn index
  = Sql.withTransaction conn $ do
      Sql.execute conn s index
      rowId <- Sql.lastInsertRowId conn
      return $ fromIntegral rowId
  where
    s = "insert into manifest (vendor, type, src_path) values (?, ?, ?)"

setManifestPathForId :: Connection -> Integer -> FilePath -> IO ()
setManifestPathForId conn mId filePath
  = Sql.execute conn s (filePath, mId)
  where
    s = "update manifest set file_path = ? where id = ?"
