module Hasql.Pool
  ( Pool,
    Settings (..),
    acquire,
    release,
    UsageError (..),
    use,
  )
where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import qualified Hasql.Pool.TimeExtras.IO as TimeExtrasIO
import Hasql.Session (Session)
import qualified Hasql.Session as Session

-- |
-- Pool of connections to DB.
data Pool
  = Pool
      Connection.Settings
      -- ^ Connection settings.
      (TQueue ActiveSlot)
      -- ^ Queue of established connections.
      (TVar Int)
      -- ^ Slots available for establishing new connections.
      (TVar Int)
      -- ^ Timestamp of the last release. Checked by connections in use to determine, whether they should be released.

-- | Available connection.
data ActiveSlot = ActiveSlot
  { activeSlotLastUseTimestamp :: Int,
    activeSlotConnection :: Connection
  }

startCollectingGarbage :: Int -> Int -> TQueue ActiveSlot -> TVar Int -> IO ()
startCollectingGarbage timeout slotsInTotal activeSlotsQueue slotsAvailVar =
  void $ forkIO go
  where
    go = do
      ts <- TimeExtrasIO.getMillisecondsSinceEpoch
      let minTs = ts - timeout
      join . atomically $ do
        tryReadTQueue activeSlotsQueue >>= \case
          Just (ActiveSlot lastUseTs connection) ->
            if lastUseTs <= minTs
              then do
                modifyTVar' slotsAvailVar succ
                return $ do
                  Connection.release connection
                  go
              else do
                unGetTQueue activeSlotsQueue $ ActiveSlot lastUseTs connection
                return $ do
                  TimeExtrasIO.sleepUntilInMilliseconds $ lastUseTs + timeout
                  go
          -- Established connections queue is empty.
          -- Let's check whether the slots avail is the same as slots in total,
          -- in which case it means that there are no connections in use,
          -- so we can stop the loop.
          Nothing -> do
            slotsAvail <- readTVar slotsAvailVar
            if slotsAvail == slotsInTotal
              then return $ return ()
              else retry

-- |
-- Settings of the connection pool. Consist of:
--
-- * Pool-size.
--
-- * Timeout.
-- An amount of time in milliseconds for which the unused connections are kept open.
--
-- * Connection settings.
type Settings =
  (Int, Int, Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (size, timeout, connectionSettings) =
  do
    establishedQueue <- newTQueueIO
    slotsAvailVar <- newTVarIO size
    lastReleaseVar <- newTVarIO 0
    return (Pool connectionSettings establishedQueue slotsAvailVar lastReleaseVar)

-- |
-- Release all connections in the pool.
-- Connections currently in use will get released right after the use.
release :: Pool -> IO ()
release (Pool _ activeSlotsQueue _ lastReleaseVar) = do
  ts <- TimeExtrasIO.getMillisecondsSinceEpoch
  activeSlots <- atomically $ do
    writeTVar lastReleaseVar ts
    flushTQueue activeSlotsQueue
  forM_ activeSlots $ Connection.release . activeSlotConnection

-- |
-- Union over the connection establishment error and the session error.
data UsageError
  = -- | Error during an attempt to connect.
    ConnectionUsageError Connection.ConnectionError
  | -- | Error during session execution.
    SessionUsageError Session.QueryError
  deriving (Show, Eq)

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished. If the session fails
-- with 'Session.ClientError' the connection will eventually get reestablished.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use (Pool connectionSettings establishedQueue slotsAvailVar lastReleaseVar) session =
  join . atomically $
    tryReadTQueue establishedQueue >>= \case
      -- No established connection avail at the moment.
      Nothing -> do
        slotsAvail <- readTVar slotsAvailVar
        -- Do we have any slots left for establishing new connections?
        case slotsAvail of
          -- Wait until the state changes and retry.
          0 -> retry
          -- Decrement the available slots var and instruct to establish and use a new connection.
          _ -> do
            writeTVar slotsAvailVar $! pred slotsAvail
            return acquireConnectionThenUseThenRegister
      Just (ActiveSlot lastUseTs connection) ->
        return (useConnectionThenRegister lastUseTs connection)
  where
    acquireConnectionThenUseThenRegister =
      Connection.acquire connectionSettings >>= \case
        -- Failed to acquire, so release an availability slot,
        -- returning the error details.
        Left acquisitionError -> do
          atomically $ modifyTVar' slotsAvailVar succ
          return (Left (ConnectionUsageError acquisitionError))
        Right connection -> do
          ts <- TimeExtrasIO.getMillisecondsSinceEpoch
          useConnectionThenRegister ts connection
    useConnectionThenRegister lastUseTs connection =
      Session.run session connection >>= \case
        Left queryError -> do
          -- Check whether the error is on client-side,
          -- and in that case release the connection.
          case queryError of
            Session.QueryError _ _ (Session.ClientError _) ->
              releaseConnection connection
            _ ->
              registerConnection lastUseTs connection
          return (Left (SessionUsageError queryError))
        Right res -> do
          registerConnection lastUseTs connection
          return (Right res)
    registerConnection lastUseTs connection =
      do
        lastReleaseTs <- readTVarIO lastReleaseVar
        if lastReleaseTs >= lastUseTs
          then releaseConnection connection
          else do
            ts <- TimeExtrasIO.getMillisecondsSinceEpoch
            atomically $ writeTQueue establishedQueue (ActiveSlot ts connection)
    releaseConnection connection =
      do
        atomically $ modifyTVar' slotsAvailVar succ
        Connection.release connection
