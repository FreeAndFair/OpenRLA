{-# LANGUAGE FlexibleInstances #-}
module TestSupport where

import           Test.Hspec.Wai (
    WaiExpectation
  , WaiSession
  , shouldRespondWith
  )
import qualified Test.Hspec.Wai as HspecWai
import           Test.Tasty.Hspec
import qualified Test.Hspec.Wai.JSON as HspecJson

import qualified Data.Aeson as A
import qualified Data.Aeson.QQ
import           Data.ByteString (ByteString)
import qualified Database.SQLite.Simple as Sql
import           Network.Wai (Application)
import           Network.Wai.Test (SResponse)
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.IO.Temp as Temp

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
setupApp = scottyApp . (mkApp False)

withApp :: ActionWith Application -> IO ()
withApp action = do
  state <- setupState
  app   <- setupApp state
  action app
  teardownState state

matchJson = HspecJson.json
json = Data.Aeson.QQ.aesonQQ

shouldRespondWithJson
  :: (A.ToJSON a)
  => WaiSession SResponse
  -> a
  -> WaiExpectation
shouldRespondWithJson action val = shouldRespondWith action matcher
  where matcher = [HspecJson.json|#{val}|]

postJson :: A.ToJSON a => ByteString -> a -> WaiSession SResponse
postJson path o = HspecWai.post path body
  where body = A.encode (A.toJSON o)
