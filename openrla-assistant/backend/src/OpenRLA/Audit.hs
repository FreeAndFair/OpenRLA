module OpenRLA.Audit where

import           Data.Function (on)
import           Data.List (maximumBy, partition)

import           OpenRLA.Types


computeRisk :: [ContestOutcome] -> [AuditMark] -> Double
computeRisk outcomes marks = (2*sW)^mW * (2 - 2*sW)^mL
  where winner = maximumBy (compare `on` coShare ) outcomes
        winnerId = coCandidateId winner
        sW = coShare winner
        voteForWinner mark = amCandidateId mark == winnerId
        (winnerVotes, loserVotes) = partition voteForWinner marks
        mW = length winnerVotes
        mL = length loserVotes
