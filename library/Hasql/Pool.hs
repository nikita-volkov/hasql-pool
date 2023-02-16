module Hasql.Pool
  ( -- * Pool
    Pool,
    withPool,
    withPoolConf,
    use,
    release,

    -- * Configuration
    Config,
    defaultConfig,
    setConnectionSettings,
    setSize,
    setMaxLifetime,
    setAcquisitionTimeout,
    setManageInterval,

    -- * Errors
    UsageError (..),
  )
where

import qualified Control.Concurrent.Async as Async
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import Hasql.Session (Session)
import qualified Hasql.Session as Session

-- | Pool configuration.
--
-- This is created by modifying 'defaultConfig' using the various setters,
-- to be passed to 'withPoolConf'.
data Config = Config
  { -- | Pool size (default 10).
    confSize :: Int,
    -- | Connection settings (default "").
    confFetchConnectionSettings :: IO Connection.Settings,
    -- | Maximal connection lifetime, in microseconds (default Nothing).
    confMaxLifetime :: Maybe Int,
    -- | Acquisition timeout, in microseconds (default Nothing).
    confAcquisitionTimeout :: Maybe Int,
    -- | Interval at which the active management thread triggers, in microseconds (default 1s).
    confManageInterval :: Int
  }

-- | Default pool configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { confSize = 10,
      confFetchConnectionSettings = pure "",
      confAcquisitionTimeout = Nothing,
      confMaxLifetime = Nothing,
      confManageInterval = 1000000 -- 1s
    }

-- | Modify a pool configuration, setting the pool size (default 10).
--
-- The pool guarantees that the number of concurrent connections never
-- exceeds this value.
setSize :: Int -> Config -> Config
setSize c config = config {confSize = c}

-- | Modify a pool configuration, setting a constant connection string. (default "").
setConnectionSettings :: Connection.Settings -> Config -> Config
setConnectionSettings s config = config {confFetchConnectionSettings = pure s}

-- | Modify a pool configuration, setting a dynamic connection string.
-- When creating a connection, the action will be run to determine the
-- current connection string.
setFetchConnectionSettings :: IO Connection.Settings -> Config -> Config
setFetchConnectionSettings s config = config {confFetchConnectionSettings = s}

-- | Modify a pool configuration, setting an acquisition timeout in microseconds. (default 'Nothing', i.e., disabled)
--
-- If this timeout is set, then acquiring a connection via 'use' will fail
-- with 'AcquisitionTimeoutUsageError' if no connection becomes available
-- within the timeout.
setAcquisitionTimeout :: Maybe Int -> Config -> Config
setAcquisitionTimeout t config = config {confAcquisitionTimeout = t}

-- | Modify a pool configuration, setting a maximum connection lifetime, in microseconds. (default 'Nothing', i.e., disabled)
--
-- If this timeout is set, then connections will be closed once they
-- are older than this value (and have been returned to the pool).
setMaxLifetime :: Maybe Int -> Config -> Config
setMaxLifetime t config = config {confMaxLifetime = t}

-- | Modify a pool configuration, setting the management thread's iteration interval, in microseconds. (Default 1s).
--
-- The pool's management thread sleeps for this interval between runs. This
-- setting affects the precision to which e.g. connection lifetime is limited
-- via 'setMaxLifetime'.
setManageInterval :: Int -> Config -> Config
setManageInterval t config = config {confManageInterval = t}

-- | A connection tagged with metadata.
data Conn = Conn
  { connConnection :: Connection,
    connCreationTimeNSec :: Word64
  }

isAlive :: Config -> Word64 -> Conn -> Bool
isAlive poolConfig now conn = case confMaxLifetime poolConfig of
  Nothing -> True
  Just lifetimeUSec -> now <= connCreationTimeNSec conn + 1000 * fromIntegral lifetimeUSec

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Configuration
    poolConfig :: Config,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Conn,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Whether to return a connection to the pool.
    poolReuse :: TVar (TVar Bool)
  }

-- | Create a connection-pool, and run the given action with it.
--
-- This uses default settings beyond the pool size and connection string.
-- Use 'withPoolConf' to modify the defaults.
withPool ::
  -- | Pool size.
  Int ->
  -- | Connection settings.
  Connection.Settings ->
  -- | Action.
  (Pool -> IO a) ->
  IO a
withPool poolSize connectionSettings = withPoolConf conf
  where
    conf = setSize poolSize . setFetchConnectionSettings (pure connectionSettings) $ defaultConfig

-- | Create a connection-pool, and run the given action with it.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- Resources associated with the pool are cleaned up afterwards, but
-- connections that have not been returned to the pool will survive.
withPoolConf ::
  -- | Pool configuration.
  Config ->
  -- | Action.
  (Pool -> IO a) ->
  IO a
withPoolConf config inner =
  bracket
    (acquire config)
    release
    (\pool -> withAsync (manage pool) (const $ inner pool))

acquire :: Config -> IO Pool
acquire config =
  Pool config
    <$> newTQueueIO
    <*> newTVarIO (confSize config)
    <*> (newTVarIO =<< newTVarIO True)

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
    prevReuse <- readTVar poolReuse
    writeTVar prevReuse False
    newReuse <- newTVar True
    writeTVar poolReuse newReuse
    conns <- flushTQueue poolConnectionQueue
    return $ forM_ conns $ \conn -> do
      Connection.release (connConnection conn)
      atomically $ modifyTVar' poolCapacity succ

-- | Active pool management thread. (For now, this closes pooled connections
-- that are older than maxLifetime.)
manage :: Pool -> IO ()
manage pool@Pool {..} = forever $ do
  threadDelay (confManageInterval poolConfig)
  now <- getMonotonicTimeNSec
  join . atomically $ do
    conns <- flushTQueue poolConnectionQueue
    let (keep, close) = partition (isAlive poolConfig now) conns
    traverse_ (writeTQueue poolConnectionQueue) keep
    return $ forM_ close $ \conn -> do
      Connection.release (connConnection conn)
      atomically $ modifyTVar' poolCapacity succ

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess = do
  timeout <- case confAcquisitionTimeout poolConfig of
    Just delta -> do
      delay <- registerDelay delta
      return $ readTVar delay
    Nothing ->
      return $ return False
  join . atomically $ do
    reuseVar <- readTVar poolReuse
    asum
      [ readTQueue poolConnectionQueue <&> onConn reuseVar,
        do
          capVal <- readTVar poolCapacity
          if capVal > 0
            then do
              writeTVar poolCapacity $! pred capVal
              return $ onNewConn reuseVar
            else retry,
        do
          timedOut <- timeout
          if timedOut
            then return . return . Left $ AcquisitionTimeoutUsageError
            else retry
      ]
  where
    onNewConn reuseVar = do
      settings <- confFetchConnectionSettings poolConfig
      now <- getMonotonicTimeNSec
      connRes <- Connection.acquire settings
      case connRes of
        Left connErr -> do
          atomically $ modifyTVar' poolCapacity succ
          return $ Left $ ConnectionUsageError connErr
        Right conn -> onLiveConn reuseVar (Conn conn now)

    onConn reuseVar conn = case confMaxLifetime poolConfig of
      Nothing -> onLiveConn reuseVar conn
      Just _ -> do
        now <- getMonotonicTimeNSec
        if isAlive poolConfig now conn
          then onLiveConn reuseVar conn
          else do
            Connection.release (connConnection conn)
            onNewConn reuseVar

    onLiveConn reuseVar conn = do
      sessRes <-
        catch (Session.run sess (connConnection conn)) $ \(err :: SomeException) -> do
          Connection.release (connConnection conn)
          atomically $ modifyTVar' poolCapacity succ
          throw err

      case sessRes of
        Left err -> case err of
          Session.QueryError _ _ (Session.ClientError _) -> do
            atomically $ modifyTVar' poolCapacity succ
            return $ Left $ SessionUsageError err
          _ -> do
            returnConn
            return $ Left $ SessionUsageError err
        Right res -> do
          returnConn
          return $ Right res
      where
        returnConn =
          join . atomically $ do
            reuse <- readTVar reuseVar
            if reuse
              then writeTQueue poolConnectionQueue conn $> return ()
              else return $ do
                Connection.release (connConnection conn)
                atomically $ modifyTVar' poolCapacity succ

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Connection.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Session.QueryError
  | -- | Timeout acquiring a connection.
    AcquisitionTimeoutUsageError
  deriving (Show, Eq)

instance Exception UsageError
