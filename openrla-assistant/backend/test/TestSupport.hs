module TestSupport where

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import qualified Database.SQLite.Simple as Sql
import           Network.Wai (Application)
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import           Control.Exception (bracket)

import           Web.Scotty (scottyApp)

import           OpenRLA (mkApp)
import qualified OpenRLA.Db as Db
import           OpenRLA.Types (State(..))


setupDataDir :: IO FilePath
setupDataDir = do
  tmpDir  <- Dir.getTemporaryDirectory
  baseDir <- Temp.createTempDirectory tmpDir "openrla_test"
  let dataDir = baseDir </> "data"
  Dir.createDirectoryIfMissing True dataDir
  return dataDir

teardownDataDir :: FilePath -> IO ()
teardownDataDir = Dir.removeDirectoryRecursive

setupState :: IO State
setupState = do
  dataDir <- setupDataDir
  let dataRel = (dataDir </>)
      dbPath  = dataRel "openrla_test.db"
  conn <- Sql.open dbPath
  Db.init conn
  return $ State { .. }

teardownState :: State -> IO ()
teardownState State { conn, dataDir } = do
  Sql.close conn
  teardownDataDir dataDir


setupApp :: State -> IO Application
setupApp = scottyApp . mkApp

withApp :: ActionWith Application -> IO ()
withApp action = do
  state <- setupState
  app   <- setupApp state
  action app
  teardownState state
