module OpenRLA.Statement.Ballot where

import           Control.Monad (forM)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement (justOneIO)
import           OpenRLA.Types



create :: Connection -> [FilePath] -> (Integer -> FilePath) -> IO [Ballot]
create conn srcPaths relPath
  = Sql.withTransaction conn $ do
      let s = "insert into ballot_image (src_path) values ?"
      forM srcPaths $ \src -> do
        Sql.execute conn s (Only src)
        rowId <- Sql.lastInsertRowId conn
        let balId       = fromIntegral rowId
            balFilePath = relPath balId
        let s' = "update ballot_image set file_path = ? where id = ?"
        Sql.execute conn s' (balFilePath, balId)
        return $ Ballot { .. }

getById :: Connection -> Integer -> IO (Maybe Ballot)
getById conn balId = do
  let s ="select id, file_path from ballot_image where id = ?"
  Sql.query conn s (Only balId) >>= justOneIO

getByOffset :: Connection -> Integer -> IO (Maybe Ballot)
getByOffset conn offset = do
  let s ="select id, file_path from ballot_image order by id limit 1 offset ?"
  Sql.query conn s (Only offset) >>= justOneIO
