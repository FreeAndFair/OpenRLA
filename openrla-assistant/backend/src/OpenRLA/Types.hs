module OpenRLA.Types where

import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:), FromJSON, ToJSON, Value(..))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text)
import           Database.SQLite.Simple (
    Connection
  , FromRow
  , SQLData(..)
  , ToRow
  , field
  , fromRow
  , toRow
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
  toJSON Regular = A.String "Regular"
  toJSON WriteIn = A.String "WriteIn"

data Ballot
  = Ballot
  { balId       :: Integer
  , balFilePath :: FilePath
  } deriving (Show, Eq)

instance FromRow Ballot where
  fromRow = do
    balId       <- field
    balFilePath <- field
    return $ Ballot { .. }

instance ToJSON Ballot where
  toJSON Ballot { .. } = A.object [ "id"       .= balId
                                  , "filePath" .= balFilePath
                                  ]

data Audit
  = Audit
  { auId         :: Integer
  , auDate       :: Text
  , auElectionId :: Integer
  , auRiskLimit  :: Double
  }
  deriving (Show, Eq)

instance ToRow Audit where
  toRow Audit { .. }
    = [ SQLInteger $ fromInteger auId
      , SQLInteger $ fromInteger auElectionId
      , SQLText    auDate
      , SQLFloat   auRiskLimit
      ]

instance FromRow Audit where
  fromRow = do
    auId         <- field
    auDate       <- field
    auElectionId <- field
    auRiskLimit  <- field

    return $ Audit { .. }

instance ToJSON Audit where
  toJSON Audit { .. } = A.object [ "id"         .= auId
                                 , "date"       .= auDate
                                 , "electionId" .= auElectionId
                                 , "riskLimit"  .= auRiskLimit
                                 ]

instance FromJSON Audit where
  parseJSON v = case v of
    Object o -> do
      auId         <- o .: "id"
      auDate       <- o .: "date"
      auElectionId <- o .: "electionId"
      auRiskLimit  <- o .: "riskLimit"
      return $ Audit { .. }
    _        -> typeMismatch "Audit" v

data AuditMark
  = AuditMark
  { amAuditId     :: Integer
  , amBallotId    :: Integer
  , amContestId   :: Integer
  , amCandidateId :: Integer
  } deriving (Show, Eq)

instance FromRow AuditMark where
  fromRow = do
    amAuditId     <- field
    amBallotId    <- field
    amContestId   <- field
    amCandidateId <- field
    return $ AuditMark { .. }

instance ToRow AuditMark where
  toRow AuditMark { .. }
    = [ SQLInteger $ fromInteger amAuditId
      , SQLInteger $ fromInteger amBallotId
      , SQLInteger $ fromInteger amContestId
      , SQLInteger $ fromInteger amCandidateId
      ]

instance ToJSON AuditMark where
  toJSON AuditMark { .. } = A.object [ "auditId"     .= amAuditId
                                     , "ballotId"    .= amBallotId
                                     , "contestId"   .= amContestId
                                     , "candidateId" .= amCandidateId
                                     ]

instance FromJSON AuditMark where
  parseJSON v = case v of
    Object o -> do
      amAuditId     <- o .: "auditId"
      amBallotId    <- o .: "ballotId"
      amContestId   <- o .: "contestId"
      amCandidateId <- o .: "candidateId"
      return $ AuditMark { .. }
    _        -> typeMismatch "AuditMark" v

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
