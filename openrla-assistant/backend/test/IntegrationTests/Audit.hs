module IntegrationTests.Audit where

import           Data.Aeson (Value(..))
import           Data.Scientific (toRealFloat)
import           Data.Vector (toList)

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import qualified IntegrationTests.Fixture as Fixture
import           JsonTestSupport
import           TestSupport
import           OpenRLA.Types


date :: String
date = "Tue Jan 01 2016 12:34:56 GMT-0000 (UTC)"

electionPostBody :: Value
electionPostBody = [json|{
  title: "The Election",
  date: #{date}
}|]

auditPostBodyA :: Value
auditPostBodyA = [json|{
  electionId: 1,
  date: #{date},
  riskLimit: 0.1,
  contests: [1001, 1003]
}|]

auditJsonA :: Value
auditJsonA = [json|{
  id: 1,
  electionId: 1,
  date: #{date},
  riskLimit: 0.1,
  contests: [
    {
      id: 1001,
      statistic: 1.0
    },
    {
      id: 1003,
      statistic: 1.0
    }
  ]
}|]

auditPostBodyB :: Value
auditPostBodyB = [json|{
  electionId: 1,
  date: #{date},
  riskLimit: 0.02,
  contests: [1002]
}|]

auditJsonB :: Value
auditJsonB = [json|{
  id: 2,
  electionId: 1,
  date: #{date},
  riskLimit: 0.02,
  contests: [
    {
      id: 1002,
      statistic: 1.0
    }
  ]
}|]

mkMarks :: Integer -> Integer -> Value
mkMarks candId1001 candId1003 = [json|[
  {
    contestId: 1001,
    candidateId: #{candId1001}
  },
  {
    contestId: 1003,
    candidateId: #{candId1003}
  }
]|]

mkMarksBody :: Integer -> Integer -> Integer -> Value
mkMarksBody ballotId candId1001 candId1003 = [json|{
  ballotId: #{ballotId},
  marks: #{mkMarks candId1001 candId1003}
}|]

spec :: Spec
spec = do
  around withApp $ context "Auditing" $ do
    it "should create an active audit" $ do
      Fixture.withElection
      Fixture.withBallots
      Fixture.withOutcomes

      get "/audit" `shouldRespondWith` "[]"
      get "/audit/1" `shouldRespondWith` 404

      auditCreateResp <- postJson "/audit" auditPostBodyA
      return auditCreateResp `shouldRespondWith` 200
      auditCreateResp `bodyShouldBe` auditJsonA

      auditIndexResp <- get "/audit"
      return auditIndexResp `shouldRespondWith` 200
      auditIndexResp `bodyShouldBe` [json|[#{auditJsonA}]|]

      auditActiveResp <- get "/audit/active"
      return auditActiveResp `shouldRespondWith` 200
      auditActiveResp `bodyShouldBe` auditJsonA

      auditByIdResp <- get "/audit/1"
      return auditByIdResp `shouldRespondWith` 200
      auditByIdResp `bodyShouldBe` auditJsonA

      get "/audit/666" `shouldRespondWith` 404

    it "should handle multiple audits" $ do
      Fixture.withElection
      Fixture.withBallots
      Fixture.withOutcomes

      postJson "/audit" auditPostBodyA
      auditCreateResp <- postJson "/audit" auditPostBodyB
      auditCreateResp `bodyShouldBe` auditJsonB

      auditIndexResp <- get "/audit"
      return auditIndexResp `shouldRespondWith` 200
      auditIndexResp `bodyShouldBe` [json|[
        #{auditJsonA},
        #{auditJsonB}
      ]|]

      auditActiveResp <- get "/audit/active"
      return auditActiveResp `shouldRespondWith` 200
      auditActiveResp `bodyShouldBe` auditJsonB

      auditByIdResp <- get "/audit/1"
      return auditByIdResp `shouldRespondWith` 200
      auditByIdResp `bodyShouldBe` auditJsonA

      auditByIdResp <- get "/audit/2"
      return auditByIdResp `shouldRespondWith` 200
      auditByIdResp `bodyShouldBe` auditJsonB

      get "/audit/666" `shouldRespondWith` 404

    it "should select a random sample and update it when marks are added" $ do
      Fixture.withElection
      Fixture.withBallots
      Fixture.withOutcomes

      postJson "/audit" auditPostBodyA

      sampleRespA <- get "/audit/1/sample"
      return sampleRespA `shouldRespondWith` 200

      let ballotIdA = getBodyId sampleRespA

      markResp <- postJson "/audit/1/marks" [json|{
        ballotId: #{ballotIdA},
        marks: #{mkMarks 1 6}
      }|]
      return markResp `shouldRespondWith` 200
      markResp `bodyShouldBe` [json|#{mkMarks 1 6}|]

      sampleRespB <- get "/audit/1/sample"
      return sampleRespB `shouldRespondWith` 200

      let ballotIdB = getBodyId sampleRespB
      liftIO $ ballotIdA `shouldNotBe` ballotIdB

      markResp <- postJson "/audit/1/marks" [json|{
        ballotId: #{ballotIdB},
        marks: #{mkMarks 2 6}
      }|]
      return markResp `shouldRespondWith` 200
      markResp `bodyShouldBe` [json|#{mkMarks 2 6}|]

      sampleRespC <- get "/audit/1/sample"
      return sampleRespC `shouldRespondWith` 200

      let ballotIdC = getBodyId sampleRespC
      liftIO $ ballotIdB `shouldNotBe` ballotIdC

    it "should set up and update statistics when marks are added" $ do
      let mkContestsJson :: Double -> Double -> Value
          mkContestsJson stat1001 stat1003 = [json|[
            { id: 1001, statistic: #{stat1001} },
            { id: 1003, statistic: #{stat1003} }
          ]|]
          statsShouldBe stat1001 stat1003 = do
            resp <- get "/audit/1"
            let Array contests = decodeBody resp .! "contests"
                [Number n1001, Number n1003] = map (.! "statistic") $ toList contests
                d1001 = toRealFloat n1001
                d1003 = toRealFloat n1003
            liftIO $ do
              let eps = 0.0000001
              abs (d1001 - stat1001) < eps `shouldBe` True
              abs (d1003 - stat1003) < eps `shouldBe` True

      Fixture.withElection
      Fixture.withBallots
      Fixture.withOutcomes

      postJson "/audit" auditPostBodyA
      statsShouldBe 1.0 1.0

      postJson "/audit/1/marks" $ mkMarksBody 1 1 6
      statsShouldBe 1.2 2.0

      postJson "/audit/1/marks" $ mkMarksBody 2 1 6
      statsShouldBe 1.44 4.0

      postJson "/audit/1/marks" $ mkMarksBody 3 2 6
      statsShouldBe 1.152 8.0

      postJson "/audit/1/marks" $ mkMarksBody 4 1 6
      statsShouldBe 1.3824 16.0

      postJson "/audit/1/marks" $ mkMarksBody 5 3 6
      statsShouldBe 1.10592 32.0

      postJson "/audit/1/marks" $ mkMarksBody 6 1 6
      statsShouldBe 1.327104 64.0

    it "should allow sampling with replacement" $ do
      let mkContestsJson :: Double -> Double -> Value
          mkContestsJson stat1001 stat1003 = [json|[
            { id: 1001, statistic: #{stat1001} },
            { id: 1003, statistic: #{stat1003} }
          ]|]
          statsShouldBe stat1001 stat1003 = do
            resp <- get "/audit/1"
            let Array contests = decodeBody resp .! "contests"
                [Number n1001, Number n1003] = map (.! "statistic") $ toList contests
                d1001 = toRealFloat n1001
                d1003 = toRealFloat n1003
            liftIO $ do
              let eps = 0.0000001
              abs (d1001 - stat1001) < eps `shouldBe` True
              abs (d1003 - stat1003) < eps `shouldBe` True

      Fixture.withElection
      Fixture.withOneBallot
      Fixture.withOutcomes

      postJson "/audit" auditPostBodyA
      statsShouldBe 1.0 1.0

      postJson "/audit/1/marks" $ mkMarksBody 1 1 6
      statsShouldBe 1.2 2.0

      postJson "/audit/1/marks" $ mkMarksBody 1 1 6
      statsShouldBe 1.44 4.0

      postJson "/audit/1/marks" $ mkMarksBody 1 2 6
      statsShouldBe 1.152 8.0

      indexResp <- get "audit/1/marks"
      let indexBody = decodeBody indexResp
      liftIO $ jsonLength indexBody `shouldBe` 3

    it "should allow fetching existing marks" $ do
      let respBalId r = balId $ decodeBody' r

      Fixture.withElection
      Fixture.withBallots
      Fixture.withOutcomes

      postJson "/audit" auditPostBodyA

      sampleResp1 <- get "/audit/1/sample"
      let balId1 = respBalId sampleResp1

      postJson "/audit/1/marks" $ mkMarksBody balId1 1 6

      sampleResp2 <- get "/audit/1/sample"
      let balId2 = respBalId sampleResp2

      postJson "/audit/1/marks" $ mkMarksBody balId2 1 6

      sampleResp3 <- get "/audit/1/sample"
      let balId3 = respBalId sampleResp3

      postJson "/audit/1/marks" $ mkMarksBody balId3 2 6

      sampleResp4 <- get "/audit/1/sample"
      let balId4 = respBalId sampleResp4

      postJson "/audit/1/marks" $ mkMarksBody balId4 1 6

      sampleResp5 <- get "/audit/1/sample"
      let balId5 = respBalId sampleResp5

      postJson "/audit/1/marks" $ mkMarksBody balId5 3 6

      sampleResp6 <- get "/audit/1/sample"
      let balId6 = respBalId sampleResp6

      postJson "/audit/1/marks" $ mkMarksBody balId6 1 6

      resp <- get "/audit/1/marks"

      return resp `shouldRespondWith` 200

      resp `bodyShouldBe` [json|[
        #{mkMarksBody balId1 1 6},
        #{mkMarksBody balId2 1 6},
        #{mkMarksBody balId3 2 6},
        #{mkMarksBody balId4 1 6},
        #{mkMarksBody balId5 3 6},
        #{mkMarksBody balId6 1 6}
      ]|]
