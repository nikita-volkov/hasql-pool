module Specs.InterruptionSpec where

import Control.Exception (SomeException, try)
import Hasql.Pool
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Config
import Hasql.Pool.Observation
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Prelude
import Test.Hspec

-- | Regression tests for the pool capacity getting lost when a pool request
-- gets interrupted anywhere between the enrollment and the session
-- (https://github.com/nikita-volkov/hasql-pool/issues/60), and for the pool
-- surviving failing observation handlers.
spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Restores the pool capacity when the pool request is interrupted while observing the connection attempt" \scopeParams ->
    onGatedPool connectingObs 1 10 1_800 1_800 scopeParams \gate pool -> do
      interruptAt gate (\pool -> void (use pool Sessions.selectOne)) pool
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Restores the pool capacity when the pool request is interrupted while observing the idle-eviction" \scopeParams ->
    onGatedPool idleEvictionObs 1 10 1_800 0.3 scopeParams \gate pool -> do
      -- Establish a connection and let it go idle.
      _ <- use pool Sessions.selectOne
      threadDelay 600_000 -- 0.6s
      interruptAt gate (\pool -> void (use pool Sessions.selectOne)) pool
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Preserves the dequeued connection when the pool request is interrupted while observing its use" \scopeParams ->
    onGatedPool inUseObs 1 10 1_800 1_800 scopeParams \gate pool -> do
      -- Establish a connection and return it to the pool.
      _ <- use pool Sessions.selectOne
      interruptAt gate (\pool -> void (use pool Sessions.selectOne)) pool
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Succeeds the session when the observation handler throws synchronously during the connection attempt" \scopeParams ->
    Scripts.onAutotaggedPoolWithObservationHandler 1 10 1_800 1_800 (throwAt connectingObs) scopeParams \_ pool -> do
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Succeeds the session when the observation handler throws synchronously during the idle-eviction" \scopeParams ->
    Scripts.onAutotaggedPoolWithObservationHandler 1 10 1_800 0.3 (throwAt idleEvictionObs) scopeParams \_ pool -> do
      _ <- use pool Sessions.selectOne
      threadDelay 600_000 -- 0.6s
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Succeeds the session when the observation handler throws synchronously while observing the use" \scopeParams ->
    Scripts.onAutotaggedPoolWithObservationHandler 1 10 1_800 1_800 (throwAt inUseObs) scopeParams \_ pool -> do
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "The reaper survives a throwing observation handler and keeps reaping on later ticks" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ counterPool ->
      Scripts.onAutotaggedPoolWithObservationHandler 1 10 1_800 0.3 (throwAt idleEvictionObs) scopeParams \appName pool -> do
        -- Establish a connection and let it go idle past the reaper's tick.
        _ <- use pool Sessions.selectOne
        threadDelay 1_600_000 -- 1.6s
        -- The connection got reaped and the reaper is still alive.
        use counterPool (Sessions.countConnections appName) `shouldReturn` Right 0
        use pool Sessions.selectOne `shouldReturn` Right 1
        -- It keeps reaping on the later ticks.
        threadDelay 1_600_000 -- 1.6s
        use counterPool (Sessions.countConnections appName) `shouldReturn` Right 0

  it "The pool remains usable after the release with a throwing observation handler" \scopeParams ->
    Scripts.onAutotaggedPoolWithObservationHandler 1 10 1_800 1_800 (throwAt releaseObs) scopeParams \_ pool -> do
      _ <- use pool Sessions.selectOne
      release pool
      use pool Sessions.selectOne `shouldReturn` Right 1

  it "Resolves the pool requests waiting on a garbage-collected pool" \scopeParams -> do
    (adapter, host, port) <- pure scopeParams
    result <- newEmptyMVar
    -- Inside this block the pool is only reachable from the waiting pool
    -- request. Once collected, the finalizer resolves it, and if it is not
    -- collectible, the acquisition timeout does. Either way it does not wait
    -- forever.
    do
      pool <-
        Pool.acquire
          adapter
          ( Config.settings
              [ -- No capacity: every pool request waits.
                Config.size 0,
                Config.acquisitionTimeout 0.5
              ]
          )
      _ <- forkIO (putMVar result =<< use pool Sessions.selectOne)
      threadDelay 100_000 -- 0.1s, to let the pool request start waiting
      pure ()
    performGC
    performGC
    res <- takeMVar result
    res `shouldBe` Left AcquisitionTimeoutUsageError

-- | Runs the action in a pool request interrupted at the gated observation.
-- The pool request is expected to die with the interruption.
interruptAt :: Gate -> (Pool.Pool -> IO ()) -> Pool.Pool -> IO ()
interruptAt gate action pool = do
  armGate gate
  done <- newEmptyMVar
  thread <- forkIO do
    _ <- try @SomeException (action pool)
    putMVar done ()
  takeMVar (gateEntered gate)
  killThread thread
  disarmGate gate
  takeMVar done

-- | Runs the continuation in a pool with a gated observation handler.
onGatedPool :: (Observation -> Bool) -> Int -> DiffTime -> DiffTime -> DiffTime -> Scripts.ScopeParams -> (Gate -> Pool.Pool -> IO ()) -> IO ()
onGatedPool matches poolSize acqTimeout maxLifetime maxIdletime scopeParams cont = do
  gate <- newGate matches
  Scripts.onAutotaggedPoolWithObservationHandler poolSize acqTimeout maxLifetime maxIdletime (gateHandler gate) scopeParams \appName pool -> cont gate pool

-- | A gate for stopping a pool request at a chosen observation and letting the
-- test interrupt it there. While the gate is armed, the pool request stops at
-- the first matching observation, waiting to be released or interrupted.
data Gate = Gate
  { gateMatches :: Observation -> Bool,
    gateEntered :: MVar (),
    gateProceed :: MVar (),
    gateArmed :: IORef Bool
  }

newGate :: (Observation -> Bool) -> IO Gate
newGate matches = Gate matches <$> newEmptyMVar <*> newEmptyMVar <*> newIORef False

armGate, disarmGate :: Gate -> IO ()
armGate gate = writeIORef (gateArmed gate) True
disarmGate gate = writeIORef (gateArmed gate) False

-- | The observation handler stopping at the gate.
gateHandler :: Gate -> Observation -> IO ()
gateHandler gate observation =
  when (gateMatches gate observation) do
    armed <- readIORef (gateArmed gate)
    when armed do
      putMVar (gateEntered gate) ()
      takeMVar (gateProceed gate)

-- | The observations the tests stop or throw at.
connectingObs, idleEvictionObs, inUseObs, releaseObs :: Observation -> Bool
connectingObs = \case
  ConnectionObservation _ ConnectingConnectionStatus -> True
  _ -> False
idleEvictionObs = \case
  ConnectionObservation _ (TerminatedConnectionStatus IdlenessConnectionTerminationReason) -> True
  _ -> False
inUseObs = \case
  ConnectionObservation _ InUseConnectionStatus -> True
  _ -> False
releaseObs = \case
  ConnectionObservation _ (TerminatedConnectionStatus ReleaseConnectionTerminationReason) -> True
  _ -> False

-- | An observation handler that throws synchronously at the matching
-- observations, and passes all the other observations through.
throwAt :: (Observation -> Bool) -> Observation -> IO ()
throwAt matches observation =
  when (matches observation) do
    error "Synchronous exception from the observation handler"
