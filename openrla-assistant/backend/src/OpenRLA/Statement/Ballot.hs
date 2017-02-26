module OpenRLA.Statement.Ballot where

import           Control.Monad (forM)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement (justOneIO)
import           OpenRLA.Types


create :: Connection -> Integer -> [FilePath] -> (Integer -> FilePath) -> IO [Ballot]
create conn elId srcPaths relPath
  = Sql.withTransaction conn $ do
      let s = "insert into ballot (src_path) values (?)"
      forM srcPaths $ \balSrcPath -> do
        Sql.execute conn s (Only balSrcPath)
        rowId <- Sql.lastInsertRowId conn
        let balId = fromIntegral rowId
            balFilePath = relPath balId
        let s' = "update ballot set file_path = ? where id = ?"
        Sql.execute conn s' (balFilePath, balId)
        let s'' = "insert into election_ballot values (?, ?)"
        Sql.execute conn s'' (elId, balId)
        return $ Ballot { .. }

createNoCopy :: Connection -> Integer -> [FilePath] -> IO [Ballot]
createNoCopy conn elId srcPaths
  = Sql.withTransaction conn $ do
      let s = "insert into ballot (src_path) values (?)"
      forM srcPaths $ \balSrcPath -> do
        Sql.execute conn s (Only balSrcPath)
        rowId <- Sql.lastInsertRowId conn
        let balId = fromIntegral rowId
            balFilePath = ""
        let s' = "update ballot set file_path = '' where id = ?"
        Sql.execute conn s' (Only balId)
        let s'' = "insert into election_ballot values (?, ?)"
        Sql.execute conn s'' (elId, balId)
        return $ Ballot { .. }

getById :: Connection -> Integer -> IO (Maybe Ballot)
getById conn balId = do
  let s ="select id, src_path, file_path from ballot where id = ?"
  Sql.query conn s (Only balId) >>= justOneIO

getByOffset :: Connection -> Integer -> IO (Maybe Ballot)
getByOffset conn offset = do
  let s ="select id, src_path, file_path from ballot order by id limit 1 offset ?"
  Sql.query conn s (Only offset) >>= justOneIO
