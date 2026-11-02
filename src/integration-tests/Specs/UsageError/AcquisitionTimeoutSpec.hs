module Specs.UsageError.AcquisitionTimeoutSpec where

import Control.Concurrent.Async (forConcurrently, race)
import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Prelude
import Test.Hspec

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

  it "Gets produced for every waiting pool request, and the pool remains usable" \scopeParams ->
    -- 100ms timeout
    Scripts.onAutotaggedPool 1 0.1 1_800 1_800 scopeParams \_ pool -> do
      held <- newEmptyMVar
      done <- newEmptyMVar
      _ <- forkIO (void (use pool (liftIO (putMVar held () *> takeMVar done))))
      takeMVar held
      results <- forConcurrently [1 .. 3 :: Int] \_ -> use pool Sessions.selectOne
      results `shouldBe` replicate 3 (Left AcquisitionTimeoutUsageError)
      putMVar done ()
      use pool Sessions.selectOne `shouldReturn` Right 1
