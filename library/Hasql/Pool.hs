module Hasql.Pool
(
  Pool,
  Settings(..),
  acquire,
  release,
  UsageError(..),
  use,
)
where

import Hasql.Pool.Prelude
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session


-- |
-- A pool of connections to DB.
data Pool =
  Pool
    Int
    {-^ Max connections. -}
    Int
    {-^ Connection timeout in milliseconds. -}
    Connection.Settings
    {-^ Connection settings. -}
    (TQueue ActiveConnection)
    {-^ Queue of active connections. -}
    (TVar Int)
    {-^ Queue size. -}
    (TVar Bool)
    {-^ Flag signaling whether pool's alive. -}

data ActiveConnection =
  ActiveConnection {
    {-| Timestamp of last use. -}
    activeConnectionLastTimestamp :: Int,
    activeConnectionConnection :: Connection
  }

loopCollectingGarbage :: Int -> TQueue ActiveConnection -> TVar Int -> TVar Bool -> IO ()
loopCollectingGarbage timeout queue queueSizeVar aliveVar =
  decide
  where
    decide =
      do
        ts <- getMillisecondsSinceEpoch
        join $ atomically $ do
          alive <- readTVar aliveVar
          if alive
            then do
              queueSize <- readTVar queueSizeVar
              if queueSize == 0
                then return (sleep (ts + timeout))
                else let
                  collect !queueSize !list =
                    do
                      entry@(ActiveConnection entryTs connection) <- readTQueue queue
                      if entryTs < ts
                        then collect (pred queueSize) (connection : list)
                        else do
                          writeTQueue queue entry
                          return (entryTs, queueSize, list)
                  in do
                    (ts, newQueueSize, list) <- collect queueSize []
                    writeTVar queueSizeVar newQueueSize
                    return (release list *> sleep ts *> decide)
            else do
              list <- flushTQueue queue
              writeTVar queueSizeVar 0
              return (release (fmap activeConnectionConnection list))
    sleep untilTs =
      do
        ts <- getMillisecondsSinceEpoch
        let
          diff =
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
-- 
type Settings =
  (Int, Int, Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (size, timeout, connectionSettings) =
  do
    queue <- newTQueueIO
    queueSizeVar <- newTVarIO 0
    aliveVar <- newTVarIO (size > 0)
    return (Pool size timeout connectionSettings queue queueSizeVar aliveVar)

-- |
-- Release the connection-pool.
release :: Pool -> IO ()
release (Pool _ _ _ _ _ aliveVar) =
  atomically (writeTVar aliveVar False)

-- |
-- A union over the connection establishment error and the session error.
data UsageError =
  ConnectionUsageError Connection.ConnectionError |
  SessionUsageError Session.QueryError |
  PoolIsReleasedUsageError
  deriving (Show, Eq)

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use (Pool _ _ connectionSettings queue queueSizeVar aliveVar) session =
  join $ atomically $ do
    alive <- readTVar aliveVar
    if alive
      then do
        queueSize <- readTVar queueSizeVar
        if queueSize > 0
          then do
            ActiveConnection _ connection <- readTQueue queue
            let
              !newQueueSize =
                succ queueSize
              in do
                writeTVar queueSizeVar newQueueSize
                return (useConnectionThenPutItToQueue connection)
          else return acquireConnectionThenUseThenPutItToQueue
      else return (return (Left PoolIsReleasedUsageError))
  where
    acquireConnectionThenUseThenPutItToQueue =
      do
        res <- Connection.acquire connectionSettings
        case res of
          Left acquisitionError -> return (Left (ConnectionUsageError acquisitionError))
          Right connection -> useConnectionThenPutItToQueue connection
    useConnectionThenPutItToQueue connection =
      do
        res <- Session.run session connection
        case res of
          Left queryError -> do
            Connection.release connection
            return (Left (SessionUsageError queryError))
          Right res -> do
            ts <- getMillisecondsSinceEpoch
            atomically $ do
              writeTQueue queue (ActiveConnection ts connection)
              modifyTVar' queueSizeVar succ
            return (Right res)

