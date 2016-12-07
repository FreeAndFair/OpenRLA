module OpenRLA.Statement.Ballot where

import           Database.SQLite.Simple (Connection)


getBallotPathById :: Connection -> Integer -> IO (Maybe FilePath)
getBallotPathById = undefined
