{-# LANGUAGE OverloadedStrings #-}
module OpenRLA (runApp, app) where

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Web.Scotty as S
import qualified Database.SQLite.Simple as Sql
import           System.Environment (getExecutablePath)
import           System.FilePath (joinPath, takeDirectory)

import qualified Action.Audit
import qualified Action.Ballot
import qualified Action.Election
import qualified Db


noop :: S.ActionM ()
noop = S.json ([] :: [Integer])

app :: Sql.Connection -> S.ScottyM ()
app conn = do
  S.middleware logStdoutDev

  S.get  "/election" $ Action.Election.index conn
  S.post "/election" $ Action.Election.create conn

  S.get "/election/:id" $ Action.Election.getById conn
  S.put "/election/:id" $ Action.Election.setById conn

  S.get "/election/active" $ Action.Election.getActive conn
  S.put "/election/active" $ Action.Election.setActive conn

  S.get  "/ballot" $ Action.Ballot.index conn
  S.post "/ballot" $ Action.Ballot.create conn

  S.get "/ballot/:id" $ Action.Ballot.getById conn
  S.put "/ballot/:id" $ Action.Ballot.setById conn

  S.get  "/audit" $ Action.Audit.index conn
  S.post "/audit" $ Action.Audit.create conn

  S.get "/audit/:id" $ Action.Audit.getById conn
  S.put "/audit/:id" $ Action.Audit.setById conn

  S.get "/audit/active" $ Action.Audit.getActive conn
  S.put "/audit/active" $ Action.Audit.setActive conn


runApp :: IO ()
runApp = do
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
      dbPath = joinPath [exeDir, "openrla.db"]
  conn <- Sql.open dbPath
  Db.init conn
  S.scotty 8080 (app conn)
