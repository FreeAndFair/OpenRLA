module OpenRLA (runApp, app) where

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Web.Scotty as S
import qualified Database.SQLite.Simple as Sql
import qualified System.Directory as D
import           System.Environment (getExecutablePath)
import           System.FilePath ((</>), takeDirectory)

import qualified OpenRLA.Controller.Audit as Audit
import qualified OpenRLA.Controller.Ballot as Ballot
import qualified OpenRLA.Controller.Election as Election
import qualified OpenRLA.Controller.Manifest as Manifest
import qualified OpenRLA.Db as Db
import           OpenRLA.Types (State(..))


app :: State -> S.ScottyM ()
app state = do
  S.middleware logStdoutDev

  S.post "/manifest" $ Manifest.create state

  S.get  "/election" $ Election.index state
  S.post "/election" $ Election.create state

  S.get "/election/:id" $ Election.getById state
  S.put "/election/:id" $ Election.setById state

  S.get "/election/:id/sample-ballot" $ Election.sampleBallot state

  S.get "/election/active" $ Election.getActive state
  S.put "/election/active" $ Election.setActive state

  S.get  "/ballot" $ Ballot.index state
  S.post "/ballot" $ Ballot.create state

  S.get "/ballot/:id" $ Ballot.getById state

  S.get  "/audit" $ Audit.index state
  S.post "/audit" $ Audit.create state

  S.get "/audit/:id" $ Audit.getById state
  S.put "/audit/:id" $ Audit.setById state

  S.get  "/audit/:id/sample" $ Audit.currentSample state

  S.get "/audit/active" $ Audit.getActive state
  S.put "/audit/active" $ Audit.setActive state


runApp :: IO ()
runApp = do
  exePath <- getExecutablePath
  let exeDir  = takeDirectory exePath
      dataDir = exeDir </> "data"
      dataRel = (dataDir </>)
      dbPath  = dataRel "openrla.db"

  D.createDirectoryIfMissing True dataDir

  conn <- Sql.open dbPath
  Db.init conn

  let state = State { .. }
  S.scotty 8080 (app state)
