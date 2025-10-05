module Specs.BySubject.UsageError.AcquisitionTimeoutSpec where

import Control.Concurrent.Async (race)
import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Gets produced on timeout" \scopeParams ->
    -- 1ms timeout
    Scripts.onAutotaggedPool 1 0.001 1_800 1_800 scopeParams \_ pool -> do
      sleeping <- newEmptyMVar
      t0 <- getCurrentTime
      res <-
        race
          ( use pool
              $ liftIO
              $ do
                putMVar sleeping ()
                -- 1s
                threadDelay 1_000_000
          )
          ( do
              takeMVar sleeping
              use pool $ Sessions.selectOne
          )
      t1 <- getCurrentTime
      res `shouldBe` Right (Left AcquisitionTimeoutUsageError)
      -- 0.5s
      diffUTCTime t1 t0 `shouldSatisfy` (< 0.5)
