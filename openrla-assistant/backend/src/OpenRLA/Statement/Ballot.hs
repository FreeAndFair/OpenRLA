module OpenRLA.Statement.Ballot where

-- import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement (justOneIO, oneRowIO)
import           OpenRLA.Types


index :: Connection -> (Integer, Integer) -> IO [Ballot]
index conn args = Sql.query conn s args
  where
    s = "select id, file_path from ballot_image limit ? offset ?"

create :: Connection -> FilePath -> (Integer -> FilePath) -> IO Ballot
create conn srcPath mkPath
  = Sql.withTransaction conn $ do
      let s = "insert into ballot_image (src_path) values ?"
      Sql.execute conn s (Only srcPath)
      rowId <- Sql.lastInsertRowId conn
      let balId       = fromIntegral rowId
          balFilePath = mkPath balId
      let s' = "update ballot_image set file_path = ? where id = ?"
      Sql.execute conn s' (balFilePath, balId)
      return $ Ballot { .. }


getById :: Connection -> Integer -> IO (Maybe FilePath)
getById = undefined

setById :: Connection -> Ballot -> IO ()
setById = undefined
