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
-- A pool of connections to DB.
data Pool
  = Pool
      Connection.Settings
      -- ^ Connection settings.
      (TQueue ActiveSlot)
      -- ^ Queue of established connections.
      (TVar Int)
      -- ^ Slots available for establishing new connections.
      (TVar Bool)
      -- ^ Flag signaling whether pool's alive.

-- | Available connection.
data ActiveSlot = ActiveSlot
  { activeSlotLastUseTimestamp :: Int,
    activeSlotConnection :: Connection
  }

loopCollectingGarbage :: Int -> TQueue ActiveSlot -> TVar Int -> TVar Bool -> IO ()
loopCollectingGarbage timeout establishedQueue slotsAvailVar aliveVar =
  decide
  where
    decide =
      do
        ts <- TimeExtrasIO.getMillisecondsSinceEpoch
        join $
          atomically $ do
            alive <- readTVar aliveVar
            if alive
              then
                let tryToRelease =
                      tryReadTQueue establishedQueue >>= \case
                        -- The queue is empty. Just wait for changes in the state.
                        Nothing ->
                          retry
                        Just entry@(ActiveSlot lastUseTs connection) ->
                          let outdatingTs =
                                lastUseTs + timeout
                           in -- Check whether it's outdated.
                              if outdatingTs < ts
                                then -- Fetch the current value of available slots and
                                -- release this one and other connections.
                                do
                                  slotsAvail <- readTVar slotsAvailVar
                                  collectAndRelease slotsAvail [connection] outdatingTs
                                else -- Return it to the front of the queue and
                                -- wait until it's outdating time.
                                do
                                  unGetTQueue establishedQueue entry
                                  return (sleep outdatingTs *> decide)
                    collectAndRelease !slotsAvail !outdatedList outdatingTs =
                      tryReadTQueue establishedQueue >>= \case
                        Nothing ->
                          finalizeAndRelease slotsAvail outdatedList outdatingTs
                        Just entry@(ActiveSlot lastUseTs connection) ->
                          let outdatingTs =
                                lastUseTs + timeout
                           in if outdatingTs < ts
                                then do
                                  unGetTQueue establishedQueue entry
                                  finalizeAndRelease slotsAvail outdatedList outdatingTs
                                else collectAndRelease (succ slotsAvail) (connection : outdatedList) outdatingTs
                    finalizeAndRelease slotsAvail outdatedList outdatingTs =
                      do
                        writeTVar slotsAvailVar slotsAvail
                        return (release outdatedList *> sleep outdatingTs *> decide)
                 in tryToRelease
              else do
                list <- flushTQueue establishedQueue
                return (release (fmap activeSlotConnection list))
    sleep untilTs =
      do
        ts <- TimeExtrasIO.getMillisecondsSinceEpoch
        let diff =
              untilTs - ts
         in if diff > 0
              then threadDelay (diff * 1000)
              else return ()
    release =
      traverse_ Connection.release

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
    aliveVar <- newTVarIO (size > 0)
    forkIO $ loopCollectingGarbage timeout establishedQueue slotsAvailVar aliveVar
    return (Pool connectionSettings establishedQueue slotsAvailVar aliveVar)

-- |
-- Release the connection-pool.
release :: Pool -> IO ()
release (Pool _ _ _ aliveVar) =
  atomically (writeTVar aliveVar False)

-- |
-- A union over the connection establishment error and the session error.
data UsageError
  = -- | Error during an attempt to connect.
    ConnectionUsageError Connection.ConnectionError
  | -- | Error during session execution.
    SessionUsageError Session.QueryError
  | -- | Pool has been released and can no longer be used.
    PoolIsReleasedUsageError
  deriving (Show, Eq)

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished. If the session fails
-- with 'Session.ClientError' the connection will eventually get reestablished.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use (Pool connectionSettings establishedQueue slotsAvailVar aliveVar) session =
  join $
    atomically $ do
      alive <- readTVar aliveVar
      if alive
        then
          tryReadTQueue establishedQueue >>= \case
            -- No established connection avail at the moment.
            Nothing -> do
              slotsAvail <- readTVar slotsAvailVar
              -- Do we have any slots left for establishing new connections?
              if slotsAvail > 0
                then -- Reduce the available slots var and instruct to
                -- establish and use a new connection.
                do
                  writeTVar slotsAvailVar $! pred slotsAvail
                  return acquireConnectionThenUseThenPutItToQueue
                else -- Wait until the state changes and retry.
                  retry
            Just (ActiveSlot _ connection) ->
              return (useConnectionThenPutItToQueue connection)
        else return (return (Left PoolIsReleasedUsageError))
  where
    acquireConnectionThenUseThenPutItToQueue =
      do
        res <- Connection.acquire connectionSettings
        case res of
          -- Failed to acquire, so release an availability slot,
          -- returning the error details.
          Left acquisitionError -> do
            atomically $ modifyTVar' slotsAvailVar succ
            return (Left (ConnectionUsageError acquisitionError))
          Right connection ->
            useConnectionThenPutItToQueue connection
    useConnectionThenPutItToQueue connection =
      do
        res <- Session.run session connection
        case res of
          Left queryError -> do
            -- Check whether the error is on client-side,
            -- and in that case release the connection.
            case queryError of
              Session.QueryError _ _ (Session.ClientError _) ->
                releaseConnection connection
              _ ->
                putConnectionToPool connection
            return (Left (SessionUsageError queryError))
          Right res -> do
            putConnectionToPool connection
            return (Right res)
    putConnectionToPool connection =
      do
        ts <- TimeExtrasIO.getMillisecondsSinceEpoch
        atomically $ writeTQueue establishedQueue (ActiveSlot ts connection)
    releaseConnection connection =
      do
        atomically $ modifyTVar' slotsAvailVar succ
        Connection.release connection
