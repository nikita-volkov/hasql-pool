module Specs.Config.InitSessionSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Does not persist after exceptions thrown in session" \scopeParams -> do
    Scripts.onAutotaggedPool 1 10 60 60 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname

      res <- use pool do
        Sessions.setSetting varName "1"
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

      try @SomeException do
        use pool do
          liftIO do
            throwIO (userError "Intentional error for testing")

      -- An asynchronous exception out of a session always spends the
      -- connection, so the pool hands out a fresh one here and the
      -- setting from before the exception is gone.
      res <- use pool do
        Sessions.getSetting varName
      shouldBe res (Right Nothing)

  it "Persists after bad query" \scopeParams -> do
    Scripts.onAutotaggedPool 1 10 60 60 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname

      res <- use pool do
        Sessions.setSetting varName "1"
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

      use pool do
        Sessions.badQuery

      res <- use pool do
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

  -- https://github.com/nikita-volkov/hasql-pool/issues/56
  it "Does not exhaust the pool capacity when it fails" \scopeParams -> do
    -- Pool of size 1 whose init session always fails, with a short
    -- acquisition timeout so that a leaked capacity slot shows up as
    -- an AcquisitionTimeoutUsageError instead of hanging the test.
    Scripts.onAutotaggedPoolWithInitSession 1 1 60 60 Sessions.badQuery scopeParams \_ pool -> do
      res1 <- use pool Sessions.selectOne
      res1 `shouldSatisfy` \case
        Left (SessionUsageError _) -> True
        _ -> False

      res2 <- use pool Sessions.selectOne
      res2 `shouldSatisfy` \case
        Left (SessionUsageError _) -> True
        _ -> False
