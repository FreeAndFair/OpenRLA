module Types where

import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:), FromJSON, ToJSON, Value(..))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text)
import           Database.SQLite.Simple (
    Connection
  , FromRow
  , SQLData(..)
  , field
  , fromRow
  )
import           Database.SQLite.Simple.FromField (
    FromField
  , fieldData
  , fromField
  )
import           Database.SQLite.Simple.Ok (Ok(..))


data State
  = State
  { conn    :: Connection
  , dataDir :: FilePath
  , dataRel :: FilePath -> FilePath
  }

data Election
  = Election
  { elId     :: Integer
  , elTitle  :: Text
  , elDate   :: Text
  , elActive :: Bool
  }
  deriving (Show, Eq)

instance FromRow Election where
  fromRow = do
    elId     <- field
    elTitle  <- field
    elDate   <- field
    elActive <- field

    return $ Election { .. }

instance ToJSON Election where
  toJSON Election { .. } =
    A.object
    [ "title"  .= elTitle
    , "date"   .= elDate
    , "active" .= elActive
    ]

data Contest
  = Contest
  { contId :: Integer
  , contExternalId :: Text
  , contDescription :: Text
  , contNumRanks :: Integer
  , contVoteFor :: Integer
  }
  deriving (Show, Eq)

instance FromRow Contest where
  fromRow = do
    contId <- field
    contExternalId <- field
    contDescription <- field
    contNumRanks <- field
    contVoteFor <- field

    return $ Contest { .. }

instance ToJSON Contest where
  toJSON Contest { .. } =
    A.object
    [ "contestId"   .= contId
    , "description" .= contDescription
    , "externalId"  .= contExternalId
    , "numRanks"    .= contNumRanks
    , "voteFor"     .= contVoteFor
    ]

data Candidate
  = Candidate
  { candId :: Integer
  , candContestId :: Integer
  , candExternalId :: Text
  , candDescription :: Text
  , candType :: CandidateType
  }
  deriving (Show, Eq)

instance FromRow Candidate where
  fromRow = do
    candId <- field
    candContestId <- field
    candExternalId <- field
    candDescription <- field
    candType <- field

    return $ Candidate { .. }

instance ToJSON Candidate where
  toJSON Candidate { .. } =
    A.object
    [ "id"          .= candId
    , "contestId"   .= candContestId
    , "externalId"  .= candExternalId
    , "description" .= candDescription
    , "type"        .= candType
    ]

data CandidateType
  = Regular
  | WriteIn
  deriving (Show, Eq)

instance FromField CandidateType where
  fromField f = case fieldData f of
    SQLText "Regular" -> Ok Regular
    SQLText "WriteIn" -> Ok WriteIn
    _ -> error "Bad conversion"

instance ToJSON CandidateType where
  toJSON Regular = A.String "regular"

data Ballot

data Audit

instance ToJSON Audit where
  toJSON _ = A.object []

data Vendor
  = Dominion
  | FreeAndFair
  deriving (Show, Eq)

instance FromField Vendor where
  fromField f = Ok $ case fieldData f of
    SQLText "dominion"    -> Dominion
    SQLText "freeandfair" -> FreeAndFair
    _ -> error "Bad conversion"

instance ToJSON Vendor where
  toJSON vendor = A.String $ case vendor of
    Dominion    -> "dominion"
    FreeAndFair -> "freeandfair"

instance FromJSON Vendor where
  parseJSON v = case v of
    Object o -> o .: "vendor"
    _        -> typeMismatch "Vendor" v
