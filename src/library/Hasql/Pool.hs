module Hasql.Pool
  ( -- * Pool
    Pool,
    acquire,
    use,
    release,

    -- * Errors
    UsageError (..),
  )
where

import Data.UUID.V4 qualified as Uuid
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Connection.Settings
import Hasql.Errors qualified as Errors
import Hasql.Pool.Config.Config qualified as Config
import Hasql.Pool.Config.ReuseOrder (ReuseOrder (..))
import Hasql.Pool.Observation
import Hasql.Pool.Prelude
import Hasql.Pool.SessionErrorDestructors qualified as ErrorsDestruction
import Hasql.Session qualified as Session
import Pqi qualified

-- | A connection tagged with metadata.
data Entry = Entry
  { entryConnection :: Connection,
    entryCreationTimeNSec :: Word64,
    entryUseTimeNSec :: Word64,
    entryId :: UUID
  }

entryIsAged :: Word64 -> Word64 -> Entry -> Bool
entryIsAged maxLifetime now Entry {..} =
  now > entryCreationTimeNSec + maxLifetime

entryIsIdle :: Word64 -> Word64 -> Entry -> Bool
entryIsIdle maxIdletime now Entry {..} =
  now > entryUseTimeNSec + maxIdletime

-- | A pool request waiting for a connection.
data Waiter = Waiter
  { -- | When the pool request times out, in nanoseconds of the monotonic clock.
    waiterDeadline :: Word64,
    -- | What the pool request gets.
    waiterGiven :: TMVar Given,
    -- | Whether the pool request stopped waiting, so that it gets passed over.
    waiterGone :: TVar Bool
  }

-- | What a waiting pool request gets: an idle connection, or the slot of a
-- closed one to establish a connection in, with the reuse flag at the time,
-- or a timeout.
data Given
  = Given (TVar Bool) (Maybe Entry)
  | TimedOut

-- | What a pool request holds after the enrollment transaction commits and
-- has to give back on any failure.
data Holding
  = -- | An entry dequeued from the idle connections.
    EntryHolding (TVar Bool) Entry
  | -- | A capacity unit taken from the pool.
    SlotHolding (TVar Bool)
  | -- | A spot in the waiting queue.
    WaitingHolding Waiter

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Pool size.
    poolSize :: Int,
    -- | Adapter used to establish connections.
    poolAdapter :: Pqi.Adapter,
    -- | Connection settings.
    poolFetchConnectionSettings :: IO Connection.Settings.Settings,
    -- | Acquisition timeout, in nanoseconds.
    poolAcquisitionTimeout :: Word64,
    -- | Maximal connection lifetime, in nanoseconds.
    poolMaxLifetime :: Word64,
    -- | Maximal connection idle time, in nanoseconds.
    poolMaxIdletime :: Word64,
    -- | Avail connections, in the order they get reused.
    poolConnectionQueue :: TQueue Entry,
    -- | Which end of the queue returned connections go to.
    poolReuseOrder :: ReuseOrder,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Pool requests waiting for a connection, the longest waiting first.
    -- Freed connections and slots go to them before anyone else,
    -- so that only the pool request given one wakes up.
    poolWaiting :: TQueue Waiter,
    -- | Whether to return a connection to the pool.
    poolReuseVar :: TVar (TVar Bool),
    -- | To stop the manager and expirer threads via garbage collection.
    poolReaperRef :: IORef (),
    -- | Action for reporting the observations.
    poolObserver :: Observation -> IO (),
    -- | Initial session to execute upon every established connection.
    poolInitSession :: Session.Session ()
  }

-- | Contain the failures of the user-supplied observation handler. Synchronous
-- exceptions from it are discarded, asynchronous ones are honored, so that
-- neither affects the pool's state or the session result, and the pool's
-- threads stay killable.
containObserver :: (Observation -> IO ()) -> Observation -> IO ()
containObserver observer observation =
  catch @SomeException (observer observation) \exc ->
    case fromException exc of
      Just SomeAsyncException {} -> throwIO exc
      Nothing -> return ()

-- | Create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- If you want to ensure that the pool connects fine at the initialization phase, just run 'use' with an empty session (@pure ()@) and check for errors.
--
-- The 'Pqi.Adapter' determines which connection implementation the pool uses, e.g. an FFI adapter backed by @postgresql-libpq@, or a pure Haskell one. Pick one from an adapter package such as @pqi-ffi@ or @pqi-native@.
acquire :: Pqi.Adapter -> Config.Config -> IO Pool
acquire adapter config = do
  connectionQueue <- newTQueueIO
  capVar <- newTVarIO (Config.size config)
  reuseVar <- newTVarIO =<< newTVarIO True
  waiting <- newTQueueIO
  reaperRef <- newIORef ()
  let observer = containObserver (Config.observationHandler config)

  managerTid <- forkIOWithUnmask $ \unmask -> unmask $ forever $ do
    threadDelay 1000000
    now <- getMonotonicTimeNSec
    join . atomically $ do
      entries <- flushTQueue connectionQueue
      let (agedEntries, unagedEntries) = partition (entryIsAged agingTimeoutNanos now) entries
          (idleEntries, liveEntries) = partition (entryIsIdle maxIdletimeNanos now) unagedEntries
      traverse_ (writeTQueue connectionQueue) liveEntries
      return $ do
        forM_ agedEntries $ reap observer reuseVar waiting capVar AgingConnectionTerminationReason
        forM_ idleEntries $ reap observer reuseVar waiting capVar IdlenessConnectionTerminationReason

  expirerTid <- forkIOWithUnmask $ \unmask -> unmask $ expire waiting

  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, resolve the pool requests still
    -- waiting, and stop the manager and the expirer.
    atomically $ drainWaiting waiting
    killThread managerTid
    killThread expirerTid

  return $ Pool (Config.size config) adapter (Config.connectionSettingsProvider config) acqTimeoutNanos agingTimeoutNanos maxIdletimeNanos connectionQueue (Config.reuseOrder config) capVar waiting reuseVar reaperRef observer (Config.initSession config)
  where
    acqTimeoutNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.acquisitionTimeout config))) 1_000
    agingTimeoutNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.agingTimeout config))) 1_000
    maxIdletimeNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.idlenessTimeout config))) 1_000

-- | Release all the idle connections in the pool, and mark the in-use connections
-- to be released after use. Any connections acquired after the call will be
-- freshly established.
--
-- The pool remains usable after this action.
-- So you can use this function to reset the connections in the pool.
-- Naturally, you can also use it to release the resources.
release :: Pool -> IO ()
release Pool {..} =
  join . atomically $ do
    prevReuse <- readTVar poolReuseVar
    writeTVar prevReuse False
    newReuse <- newTVar True
    writeTVar poolReuseVar newReuse
    entries <- flushTQueue poolConnectionQueue
    return $ forM_ entries $ \entry ->
      mask \restore ->
        catch @SomeException
          ( do
              restore $ Connection.release (entryConnection entry)
              restore $ poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))
              atomically $ freeSlot poolReuseVar poolWaiting poolCapacity
          )
          \exc -> do
            atomically $ freeSlot poolReuseVar poolWaiting poolCapacity
            case fromException exc of
              Just SomeAsyncException {} -> throwIO exc
              Nothing -> return ()

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
--
-- __Warning:__ Due to the mechanism mentioned above you should avoid intercepting this error type from within sessions.
--
-- When no connection is available, the pool requests wait for one in the
-- order they came, and a later pool request doesn't take a connection while
-- others wait.
use :: forall a. Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess = do
  now <- getMonotonicTimeNSec
  mask \unmask -> do
    holding <- atomically $ do
      reuseVar <- readTVar poolReuseVar
      waited <- someoneWaiting poolWaiting
      -- Behind the pool requests already waiting, which get what's freed first.
      if waited
        then enqueue now <&> WaitingHolding
        else
          asum
            [ readTQueue poolConnectionQueue <&> EntryHolding reuseVar,
              do
                capVal <- readTVar poolCapacity
                if capVal > 0
                  then do
                    writeTVar poolCapacity $! pred capVal
                    return $ SlotHolding reuseVar
                  else retry,
              enqueue now <&> WaitingHolding
            ]
    case holding of
      EntryHolding reuseVar entry -> onEntry unmask reuseVar entry
      SlotHolding reuseVar -> onSlot unmask reuseVar
      WaitingHolding waiter -> do
        given <- atomically (takeTMVar (waiterGiven waiter)) `onException` abandon waiter
        case given of
          TimedOut -> return $ Left AcquisitionTimeoutUsageError
          Given reuseVar Nothing -> onSlot unmask reuseVar
          Given reuseVar (Just entry) -> onEntry unmask reuseVar entry
  where
    enqueue :: Word64 -> STM Waiter
    enqueue now = do
      waiter <- Waiter (now + poolAcquisitionTimeout) <$> newEmptyTMVar <*> newTVar False
      writeTQueue poolWaiting waiter
      return waiter

    abandon :: Waiter -> IO ()
    abandon waiter = do
      given <- atomically $ do
        writeTVar (waiterGone waiter) True
        tryTakeTMVar (waiterGiven waiter)
      forM_ given \case
        Given _ Nothing -> atomically returnSlot
        Given reuseVar (Just entry) -> returnConn reuseVar entry
        TimedOut -> return ()

    returnSlot :: STM ()
    returnSlot = freeSlot poolReuseVar poolWaiting poolCapacity

    -- | Holding one entry dequeued from the idle connections. If it is still
    -- good, run the session on it; if it expired, replace it with a fresh
    -- connection on the same slot. All the way, the holding is given back on
    -- interruption.
    onEntry :: (forall x. IO x -> IO x) -> TVar Bool -> Entry -> IO (Either UsageError a)
    onEntry unmask reuseVar entry = do
      now <- getMonotonicTimeNSec
      if entryIsAged poolMaxLifetime now entry
        then renewEntry unmask reuseVar entry AgingConnectionTerminationReason
        else
          if entryIsIdle poolMaxIdletime now entry
            then renewEntry unmask reuseVar entry IdlenessConnectionTerminationReason
            else onLiveConn unmask reuseVar entry {entryUseTimeNSec = now}

    -- | Holding an expired entry. Release its connection and establish a
    -- fresh one on the same slot. On interruption the slot gets given back:
    -- the released connection cannot return to the pool.
    renewEntry :: (forall x. IO x -> IO x) -> TVar Bool -> Entry -> ConnectionTerminationReason -> IO (Either UsageError a)
    renewEntry unmask reuseVar entry reason = do
      outcome <-
        onException
          ( do
              Connection.release (entryConnection entry)
              unmask $ poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus reason))
              establish unmask
          )
          (atomically returnSlot)
      case outcome of
        Left err -> return $ Left err
        Right newEntry -> onLiveConn unmask reuseVar newEntry

    -- | Holding one capacity unit. Establish a connection on it and run the
    -- session. On interruption the slot gets given back.
    onSlot :: (forall x. IO x -> IO x) -> TVar Bool -> IO (Either UsageError a)
    onSlot unmask reuseVar = do
      outcome <- onException (establish unmask) (atomically returnSlot)
      case outcome of
        Left err -> return $ Left err
        Right entry -> onLiveConn unmask reuseVar entry

    -- Run with one capacity unit held: establish the connection and run the
    -- init session on it. On any failure the slot gets given back and the
    -- error gets returned. On interruption the slot gets given back and the
    -- exception gets rethrown.
    establish :: (forall x. IO x -> IO x) -> IO (Either UsageError Entry)
    establish unmask = do
      settings <- unmask poolFetchConnectionSettings
      now <- getMonotonicTimeNSec
      id <- Uuid.nextRandom
      unmask $ poolObserver (ConnectionObservation id ConnectingConnectionStatus)
      unmask (Connection.acquire poolAdapter settings) >>= \case
        Left connErr -> do
          let connErrText = case connErr of
                Errors.NetworkingConnectionError details -> Just details
                Errors.AuthenticationConnectionError details -> Just details
                Errors.CompatibilityConnectionError details -> Just details
                Errors.OtherConnectionError details -> if details == "" then Nothing else Just details
          unmask $ poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason connErrText)))
          atomically returnSlot
          return $ Left $ ConnectionUsageError connErr
        Right connection ->
          unmask (Connection.use connection poolInitSession) >>= \case
            Left err -> do
              Connection.release connection
              unmask $
                ErrorsDestruction.reset
                  (\details -> poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (Just details)))))
                  (poolObserver (ConnectionObservation id (TerminatedConnectionStatus (InitializationErrorTerminationReason err))))
                  err
              atomically returnSlot
              return $ Left $ SessionUsageError err
            Right () -> do
              unmask $ poolObserver (ConnectionObservation id (ReadyForUseConnectionStatus EstablishedConnectionReadyForUseReason))
              return $ Right (Entry connection now now id)

    -- | Holding one entry ready for use. Run the session on it, return the
    -- entry to the pool, and return the session result. Interrupted, the
    -- session exception gets rethrown after the entry is back in the pool.
    onLiveConn :: (forall x. IO x -> IO x) -> TVar Bool -> Entry -> IO (Either UsageError a)
    onLiveConn unmask reuseVar entry = do
      sessRes <-
        onException
          ( unmask $ do
              poolObserver (ConnectionObservation (entryId entry) InUseConnectionStatus)
              try @SomeException (Connection.use (entryConnection entry) sess)
          )
          (returnConn reuseVar entry)

      case sessRes of
        Left exc -> do
          returnConn reuseVar entry
          throwIO exc
        Right (Left err) ->
          if ErrorsDestruction.requiresConnectionDiscard err
            then do
              Connection.release (entryConnection entry)
              atomically returnSlot
              unmask $
                poolObserver
                  ( ConnectionObservation
                      (entryId entry)
                      (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (ErrorsDestruction.discardDetails err)))
                  )
              return $ Left $ SessionUsageError err
            else do
              returnConn reuseVar entry
              unmask $ poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus (SessionFailedConnectionReadyForUseReason err)))
              return $ Left $ SessionUsageError err
        Right (Right res) -> do
          returnConn reuseVar entry
          unmask $ poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus SessionSucceededConnectionReadyForUseReason))
          return $ Right res

    -- Back to the pool request waiting longest or to the idle connections,
    -- unless the pool was released meanwhile.
    returnConn reuseVar entry =
      join . atomically $ do
        reuse <- readTVar reuseVar
        if reuse
          then do
            nextWaiter poolWaiting >>= \case
              Just waiter -> do
                currentReuseVar <- readTVar poolReuseVar
                putTMVar (waiterGiven waiter) (Given currentReuseVar (Just entry))
              Nothing ->
                case poolReuseOrder of
                  LIFO -> unGetTQueue poolConnectionQueue entry
                  FIFO -> writeTQueue poolConnectionQueue entry
            return $ return ()
          else return $ do
            Connection.release (entryConnection entry)
            atomically returnSlot
            poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))

-- | Whether a pool request is waiting. The ones that stopped waiting get
-- dropped.
someoneWaiting :: TQueue Waiter -> STM Bool
someoneWaiting waiting =
  tryPeekTQueue waiting >>= \case
    Nothing -> return False
    Just waiter ->
      readTVar (waiterGone waiter) >>= \case
        True -> readTQueue waiting *> someoneWaiting waiting
        False -> return True

-- | Take the pool request waiting longest, passing over the ones that stopped
-- waiting.
nextWaiter :: TQueue Waiter -> STM (Maybe Waiter)
nextWaiter waiting =
  tryReadTQueue waiting >>= \case
    Nothing -> return Nothing
    Just waiter ->
      readTVar (waiterGone waiter) >>= \case
        True -> nextWaiter waiting
        False -> return (Just waiter)

-- | Give the slot of a closed connection to the pool request waiting longest,
-- or back to the pool if no pool request waits.
freeSlot :: TVar (TVar Bool) -> TQueue Waiter -> TVar Int -> STM ()
freeSlot reuseVarVar waiting capVar =
  nextWaiter waiting >>= \case
    Just waiter -> do
      reuseVar <- readTVar reuseVarVar
      putTMVar (waiterGiven waiter) (Given reuseVar Nothing)
    Nothing -> modifyTVar' capVar succ

-- | Time out the pool requests waiting too long. All wait as long, so the pool
-- request waiting longest times out first: one thread sleeps until its
-- deadline, rather than every waiting pool request registering a timer, which
-- GHC can't cancel.
expire :: TQueue Waiter -> IO ()
expire waiting = forever $ do
  deadline <- atomically $ do
    waited <- someoneWaiting waiting
    if waited then waiterDeadline <$> peekTQueue waiting else retry
  now <- getMonotonicTimeNSec
  -- In microseconds, at most an hour at a time.
  when (deadline > now) $ threadDelay (fromIntegral (min 3_600_000_000 (div (deadline - now) 1_000 + 1)))
  atomically . timeOut =<< getMonotonicTimeNSec
  where
    timeOut now =
      tryPeekTQueue waiting >>= \case
        Just waiter | waiterDeadline waiter <= now -> do
          _ <- readTQueue waiting
          gone <- readTVar (waiterGone waiter)
          unless gone $ putTMVar (waiterGiven waiter) TimedOut
          timeOut now
        _ -> return ()

-- | Resolve the pool requests still waiting: with the pool gone, nothing will
-- ever be given to them.
drainWaiting :: TQueue Waiter -> STM ()
drainWaiting waiting =
  tryReadTQueue waiting >>= \case
    Nothing -> return ()
    Just waiter -> do
      gone <- readTVar (waiterGone waiter)
      unless gone $ void $ tryPutTMVar (waiterGiven waiter) TimedOut
      drainWaiting waiting

-- | Terminate one expired connection on the reaper's tick: release it, report
-- the termination, and give its slot back. The slot gets given back even when
-- the release or the observation fails, so no capacity is lost, and a failing
-- entry neither skips the other entries of the tick nor kills the reaper.
reap :: (Observation -> IO ()) -> TVar (TVar Bool) -> TQueue Waiter -> TVar Int -> ConnectionTerminationReason -> Entry -> IO ()
reap observer reuseVar waiting capVar reason entry =
  mask \restore ->
    catch @SomeException
      ( do
          restore $ Connection.release (entryConnection entry)
          restore $ observer (ConnectionObservation (entryId entry) (TerminatedConnectionStatus reason))
          atomically $ freeSlot reuseVar waiting capVar
      )
      \exc -> do
        atomically $ freeSlot reuseVar waiting capVar
        case fromException exc of
          Just SomeAsyncException {} -> throwIO exc
          Nothing -> return ()

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Errors.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Errors.SessionError
  | -- | Timeout acquiring a connection.
    AcquisitionTimeoutUsageError
  deriving (Show, Eq)

instance Exception UsageError
