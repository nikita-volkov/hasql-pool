module Hasql.Pool
  ( -- * Pool
    Pool,
    acquire,
    acquireConf,
    acquireDynamically,
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

import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
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
    -- | Maximal connection lifetime, in microseconds (default 30m).
    confMaxLifetime :: Int,
    -- | Acquisition timeout, in microseconds (default 10s).
    confAcquisitionTimeout :: Int,
    -- | Interval at which the active management thread triggers, in microseconds (default 1s).
    confManageInterval :: Int
  }

-- | Default pool configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { confSize = 10,
      confFetchConnectionSettings = pure "",
      confAcquisitionTimeout = 10 * oneSecond,
      confMaxLifetime = 30 * oneMinute,
      confManageInterval = oneSecond
    }
  where
    oneSecond = 1000000
    oneMinute = 60 * oneSecond

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

-- | Modify a pool configuration, setting the acquisition timeout in microseconds. (default 'Nothing', i.e., disabled)
--
-- Acquiring a connection via 'use' will fail with 'AcquisitionTimeoutUsageError'
-- if no connection becomes available within the timeout.
setAcquisitionTimeout :: Int -> Config -> Config
setAcquisitionTimeout t config = config {confAcquisitionTimeout = t}

-- | Modify a pool configuration, setting the maximum connection lifetime, in microseconds. (default 'Nothing', i.e., disabled)
--
-- Connections will be closed once they are older than this value
-- (and have been returned to the pool).
setMaxLifetime :: Int -> Config -> Config
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
isAlive poolConfig now conn =
  now <= connCreationTimeNSec conn + 1000 * fromIntegral (confMaxLifetime poolConfig)

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
    poolReuse :: TVar (TVar Bool),
    -- | To stop the manager thread via garbage collection.
    poolReaperRef :: IORef ()
  }

-- | Create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
acquireConf :: Config -> IO Pool
acquireConf config = do
  connectionQueue <- newTQueueIO
  capacity <- newTVarIO (confSize config)
  reuse <- newTVarIO =<< newTVarIO True
  reaperRef <- newIORef ()

  manager <- forkIOWithUnmask $ \unmask -> unmask $ manage config connectionQueue capacity
  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, stop the manager.
    killThread manager

  return $ Pool config connectionQueue capacity reuse reaperRef

-- | Create a connection-pool, with default settings.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
acquire ::
  -- | Pool size.
  Int ->
  -- | Connection acquisition timeout in microseconds.
  Maybe Int ->
  -- | Connection settings.
  Connection.Settings ->
  IO Pool
acquire poolSize acqTimeout connectionSettings =
  acquireDynamically poolSize acqTimeout (pure connectionSettings)

-- | Create a connection-pool.
--
-- In difference to 'acquire' new connection settings get fetched each
-- time a connection is created. This may be useful for some security models.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
acquireDynamically ::
  -- | Pool size.
  Int ->
  -- | Connection acquisition timeout in microseconds.
  Maybe Int ->
  -- | Action fetching connection settings.
  IO Connection.Settings ->
  IO Pool
acquireDynamically poolSize acqTimeout fetchConnectionSettings =
  acquireConf
    . setSize poolSize
    . setFetchConnectionSettings fetchConnectionSettings
    . maybe id setAcquisitionTimeout acqTimeout
    $ defaultConfig

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
manage :: Config -> TQueue Conn -> TVar Int -> IO ()
manage config connectionQueue capacity = forever $ do
  threadDelay (confManageInterval config)
  now <- getMonotonicTimeNSec
  join . atomically $ do
    conns <- flushTQueue connectionQueue
    let (keep, close) = partition (isAlive config now) conns
    traverse_ (writeTQueue connectionQueue) keep
    return $ forM_ close $ \conn -> do
      Connection.release (connConnection conn)
      atomically $ modifyTVar' capacity succ

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess = do
  timeout <- do
    delay <- registerDelay $ confAcquisitionTimeout poolConfig
    return $ readTVar delay
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

    onConn reuseVar conn = do
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
