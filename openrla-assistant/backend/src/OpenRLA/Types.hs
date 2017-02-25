module OpenRLA.Types where

import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:), FromJSON, ToJSON, Value(..))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text, unpack)
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
    elId      <- field
    elTitle   <- field
    elDate    <- field
    elActiveF <- field

    let elActive = maybe False toTrue elActiveF
        toTrue :: Integer -> Bool
        toTrue = const True

    return $ Election { .. }

instance ToRow Election where
  toRow Election { .. }
    = [ SQLInteger $ fromInteger elId
      , SQLText    elTitle
      , SQLText    elDate
      ]

instance ToJSON Election where
  toJSON Election { .. } =
    A.object
    [ "id"     .= elId
    , "title"  .= elTitle
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
    [ "id"   .= contId
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
    SQLText t -> error $ "Bad conversion, can't parse text '" ++ unpack t ++ "'"
    _ -> error "Bad conversion, can't parse type"

instance ToJSON CandidateType where
  toJSON Regular = A.String "Regular"
  toJSON WriteIn = A.String "WriteIn"

data Ballot
  = Ballot
  { balId       :: Integer
  , balFilePath :: FilePath
  , balSrcPath  :: FilePath
  } deriving (Show, Eq)

instance FromRow Ballot where
  fromRow = do
    balId       <- field
    balSrcPath  <- field
    balFilePath <- field
    return $ Ballot { .. }

instance ToJSON Ballot where
  toJSON Ballot { .. } = A.object [ "id"       .= balId
                                  , "filePath" .= balFilePath
                                  , "srcPath"  .= balSrcPath
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
    auElectionId <- field
    auDate       <- field
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

data AuditSample
  = AuditSample
  { ausId       :: Integer
  , ausAuditId  :: Integer
  , ausBallotId :: Integer
  }
  deriving (Show, Eq)

instance ToRow AuditSample where
  toRow AuditSample { .. }
    = [ SQLInteger $ fromInteger ausId
      , SQLInteger $ fromInteger ausAuditId
      , SQLInteger $ fromInteger ausBallotId
      ]

instance FromRow AuditSample where
  fromRow = do
    ausId       <- field
    ausAuditId  <- field
    ausBallotId <- field

    return $ AuditSample { .. }

data AuditMark
  = AuditMark
  { amAuditId     :: Integer
  , amBallotId    :: Integer
  , amContestId   :: Integer
  , amCandidateId :: Integer
  , amSampleId    :: Integer
  } deriving (Show, Eq)

instance FromRow AuditMark where
  fromRow = do
    amAuditId     <- field
    amBallotId    <- field
    amContestId   <- field
    amCandidateId <- field
    amSampleId    <- field
    return $ AuditMark { .. }

instance ToRow AuditMark where
  toRow AuditMark { .. }
    = [ SQLInteger $ fromInteger amAuditId
      , SQLInteger $ fromInteger amBallotId
      , SQLInteger $ fromInteger amContestId
      , SQLInteger $ fromInteger amCandidateId
      , SQLInteger $ fromInteger amSampleId
      ]

instance ToJSON AuditMark where
  toJSON AuditMark { .. } = A.object [ "auditId"     .= amAuditId
                                     , "ballotId"    .= amBallotId
                                     , "contestId"   .= amContestId
                                     , "candidateId" .= amCandidateId
                                     , "sampleId"    .= amSampleId
                                     ]

instance FromJSON AuditMark where
  parseJSON v = case v of
    Object o -> do
      amAuditId     <- o .: "auditId"
      amBallotId    <- o .: "ballotId"
      amContestId   <- o .: "contestId"
      amCandidateId <- o .: "candidateId"
      amSampleId    <- o .: "sampleId"
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

data ContestOutcome
  = ContestOutcome
  { coElectionId  :: Integer
  , coContestId   :: Integer
  , coCandidateId :: Integer
  , coShare       :: Double
  } deriving (Show, Eq)

instance FromRow ContestOutcome where
  fromRow = do
    coElectionId  <- field
    coContestId   <- field
    coCandidateId <- field
    coShare       <- field
    return $ ContestOutcome { .. }

instance ToRow ContestOutcome where
  toRow ContestOutcome { .. }
    = [ SQLInteger $ fromInteger coElectionId
      , SQLInteger $ fromInteger coContestId
      , SQLInteger $ fromInteger coCandidateId
      , SQLFloat   $ coShare
      ]

instance ToJSON ContestOutcome where
  toJSON ContestOutcome { .. }
    = A.object [ "electionId"  .= coElectionId
               , "contestId"   .= coContestId
               , "candidateId" .= coCandidateId
               , "share"       .= coShare
               ]

instance FromJSON ContestOutcome where
  parseJSON v = case v of
    Object o -> do
      coElectionId  <- o .: "electionId"
      coContestId   <- o .: "contestId"
      coCandidateId <- o .: "candidateId"
      coShare       <- o .: "share"
      return $ ContestOutcome { .. }
    _        -> typeMismatch "AuditMark" v
