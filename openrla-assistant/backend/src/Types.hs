{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import qualified Data.Aeson as A
import           Data.Aeson ((.=), ToJSON)
import qualified Data.Text as T
import           Database.SQLite.Simple (
    FromRow
  -- , ResultError (..)
  , SQLData(..)
  , field
  , fromRow
  )
import           Database.SQLite.Simple.FromField (FromField, fieldData, fromField)
import           Database.SQLite.Simple.Ok


data Election
  = Election
  { elecTitle :: T.Text
  , elecCandidates :: [Candidate]
  , elecContests :: [Contest]
  , elecDate :: T.Text
  }
  deriving (Show, Eq)

instance ToJSON Election where
  toJSON Election { .. } =
    A.object
    [ "title"      .= elecTitle
    , "date"       .= elecDate
    , "candidates" .= elecCandidates
    , "contests"   .= elecContests
    ]

data Contest
  = Contest
  { contId :: Integer
  , contExternalId :: T.Text
  , contDescription :: T.Text
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
  , candExternalId :: T.Text
  , candDescription :: T.Text
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
  deriving (Show, Eq)

instance FromField CandidateType where
  fromField f = case fieldData f of
    SQLText "regular" -> Ok Regular
    _ -> error "Bad conversion"

instance ToJSON CandidateType where
  toJSON Regular = A.String "regular"

data Ballot

data Audit

instance ToJSON Audit where
  toJSON _ = A.object []
