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

import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.UUID.V4 qualified as Uuid
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Pool.Config.Config qualified as Config
import Hasql.Pool.Observation
import Hasql.Pool.Prelude
import Hasql.Pool.SessionErrorDestructors qualified as ErrorsDestruction
import Hasql.Session qualified as Session

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

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Pool size.
    poolSize :: Int,
    -- | Connection settings.
    poolFetchConnectionSettings :: IO [Connection.Setting.Setting],
    -- | Acquisition timeout, in microseconds.
    poolAcquisitionTimeout :: Int,
    -- | Maximal connection lifetime, in nanoseconds.
    poolMaxLifetime :: Word64,
    -- | Maximal connection idle time, in nanoseconds.
    poolMaxIdletime :: Word64,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Entry,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Whether to return a connection to the pool.
    poolReuseVar :: TVar (TVar Bool),
    -- | To stop the manager thread via garbage collection.
    poolReaperRef :: IORef (),
    -- | Action for reporting the observations.
    poolObserver :: Observation -> IO (),
    -- | Initial session to execute upon every established connection.
    poolInitSession :: Session.Session ()
  }

-- | Create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- If you want to ensure that the pool connects fine at the initialization phase, just run 'use' with an empty session (@pure ()@) and check for errors.
acquire :: Config.Config -> IO Pool
acquire config = do
  connectionQueue <- newTQueueIO
  capVar <- newTVarIO (Config.size config)
  reuseVar <- newTVarIO =<< newTVarIO True
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
        forM_ agedEntries $ reap observer capVar AgingConnectionTerminationReason
        forM_ idleEntries $ reap observer capVar IdlenessConnectionTerminationReason

  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, stop the manager.
    killThread managerTid

  return $ Pool (Config.size config) (Config.connectionSettingsProvider config) acqTimeoutMicros agingTimeoutNanos maxIdletimeNanos connectionQueue capVar reuseVar reaperRef observer (Config.initSession config)
  where
    acqTimeoutMicros =
      div (fromIntegral (diffTimeToPicoseconds (Config.acquisitionTimeout config))) 1_000_000
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
              atomically $ modifyTVar' poolCapacity succ
          )
          \exc -> do
            atomically $ modifyTVar' poolCapacity succ
            case fromException exc of
              Just SomeAsyncException {} -> throwIO exc
              Nothing -> return ()

-- | Terminate one expired connection on the reaper's tick: release it, report
-- the termination, and give its slot back. The slot gets given back even when
-- the release or the observation fails, so no capacity is lost, and a failing
-- entry neither skips the other entries of the tick nor kills the reaper.
reap :: (Observation -> IO ()) -> TVar Int -> ConnectionTerminationReason -> Entry -> IO ()
reap observer capVar reason entry =
  mask \restore ->
    catch @SomeException
      ( do
          restore $ Connection.release (entryConnection entry)
          restore $ observer (ConnectionObservation (entryId entry) (TerminatedConnectionStatus reason))
          atomically $ modifyTVar' capVar succ
      )
      \exc -> do
        atomically $ modifyTVar' capVar succ
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
use :: forall a. Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess =
  mask \unmask -> do
    timeout <- do
      delay <- registerDelay poolAcquisitionTimeout
      return $ readTVar delay
    holding <- atomically $ do
      reuseVar <- readTVar poolReuseVar
      asum
        [ readTQueue poolConnectionQueue <&> EntryHolding reuseVar,
          do
            capVal <- readTVar poolCapacity
            if capVal > 0
              then do
                writeTVar poolCapacity $! pred capVal
                return $ SlotHolding reuseVar
              else retry,
          do
            timedOut <- timeout
            if timedOut
              then return TimedOutHolding
              else retry
        ]
    case holding of
      TimedOutHolding -> return $ Left AcquisitionTimeoutUsageError
      EntryHolding reuseVar entry -> onEntry unmask reuseVar entry
      SlotHolding reuseVar -> onSlot unmask reuseVar
  where
    returnSlot :: STM ()
    returnSlot = modifyTVar' poolCapacity succ

    -- \| Holding one entry dequeued from the idle connections. If it is still
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

    -- \| Holding an expired entry. Release its connection and establish a
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

    -- \| Holding one capacity unit. Establish a connection on it and run the
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
      unmask (Connection.acquire settings) >>= \case
        Left connErr -> do
          unmask $ poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (fmap (Text.decodeUtf8With Text.lenientDecode) connErr))))
          atomically returnSlot
          return $ Left $ ConnectionUsageError connErr
        Right connection ->
          unmask (Session.run poolInitSession connection) >>= \case
            Left err -> do
              Connection.release connection
              unmask
                $ ErrorsDestruction.reset
                  ( \details ->
                      poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (fmap (Text.decodeUtf8With Text.lenientDecode) details))))
                  )
                  (poolObserver (ConnectionObservation id (TerminatedConnectionStatus (InitializationErrorTerminationReason err))))
                  err
              atomically returnSlot
              return $ Left $ SessionUsageError err
            Right () -> do
              unmask $ poolObserver (ConnectionObservation id (ReadyForUseConnectionStatus EstablishedConnectionReadyForUseReason))
              return $ Right (Entry connection now now id)

    -- \| Holding one entry ready for use. Run the session on it, return the
    -- entry to the pool, and return the session result. Interrupted, the
    -- session exception gets rethrown after the entry is back in the pool.
    onLiveConn :: (forall x. IO x -> IO x) -> TVar Bool -> Entry -> IO (Either UsageError a)
    onLiveConn unmask reuseVar entry = do
      sessRes <-
        onException
          ( unmask $ do
              poolObserver (ConnectionObservation (entryId entry) InUseConnectionStatus)
              try @SomeException (Session.run sess (entryConnection entry))
          )
          (returnConn reuseVar entry)

      case sessRes of
        Left exc -> do
          returnConn reuseVar entry
          throwIO exc
        Right (Left err) ->
          ErrorsDestruction.reset
            ( \details -> do
                Connection.release (entryConnection entry)
                atomically returnSlot
                unmask $ poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (fmap (Text.decodeUtf8With Text.lenientDecode) details))))
                return $ Left $ SessionUsageError err
            )
            ( do
                returnConn reuseVar entry
                unmask $ poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus (SessionFailedConnectionReadyForUseReason err)))
                return $ Left $ SessionUsageError err
            )
            err
        Right (Right res) -> do
          returnConn reuseVar entry
          unmask $ poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus SessionSucceededConnectionReadyForUseReason))
          return $ Right res

    -- Back to the idle connections, unless the pool was released meanwhile.
    returnConn :: TVar Bool -> Entry -> IO ()
    returnConn reuseVar entry =
      join . atomically $ do
        reuse <- readTVar reuseVar
        if reuse
          then writeTQueue poolConnectionQueue entry $> return ()
          else return $ do
            Connection.release (entryConnection entry)
            atomically returnSlot
            poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))

-- | What a pool request holds after the enrollment transaction commits and
-- has to give back on any failure.
data Holding
  = -- | An entry dequeued from the idle connections.
    EntryHolding (TVar Bool) Entry
  | -- | A capacity unit taken from the pool.
    SlotHolding (TVar Bool)
  | -- | The acquisition timed out.
    TimedOutHolding

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Connection.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Session.SessionError
  | -- | Timeout acquiring a connection.
    AcquisitionTimeoutUsageError
  deriving (Show, Eq)

instance Exception UsageError
