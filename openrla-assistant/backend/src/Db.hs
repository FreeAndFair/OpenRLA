{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Db (
    Db.init
  ) where

import qualified Data.Text as T
import           Data.String.Here
import           Database.SQLite.Simple (Connection (..))
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as DSql


schema :: T.Text
schema = T.pack [hereFile|sql/schema.sql|]

executeScript :: Connection -> T.Text -> IO ()
executeScript = DSql.exec . connectionHandle

init :: Connection -> IO ()
init conn = executeScript conn schema
