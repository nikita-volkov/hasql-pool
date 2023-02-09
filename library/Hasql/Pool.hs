module Hasql.Pool
  ( -- * Pool
    Config,
    defaultConfig,
    setCapacity,
    setConnectionSettings,
    setMaxLifetime,
    setAcquisitionTimeout,
    setManageInterval,
    Pool,
    acquireConf,
    acquire,
    acquireDynamically,
    withManagedPool,
    release,
    use,

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


data Conn = Conn
  { connConnection :: Connection,
    connCreationTimeNSec :: Word64
  }

data Config = Config -- Settings ??
  { confCapacity :: Int,
    -- | Connection settings.
    confFetchConnectionSettings :: IO Connection.Settings,
    -- | Maximal connection lifetime, in microseconds.
    confMaxLifetime :: Maybe Int,
    -- | Acquisition timeout, in microseconds.
    confAcquisitionTimeout :: Maybe Int,
    confManageInterval :: Int -- microseconds
  }

defaultConfig :: Config
defaultConfig = Config
  { confCapacity = 10,
    confFetchConnectionSettings = pure "host=localhost port=5432 user=postgres database=postgres",
    confAcquisitionTimeout = Nothing,
    confMaxLifetime = Nothing,
    confManageInterval = 1000000 -- 1s
  }

setCapacity :: Int -> Config -> Config
setCapacity c config = config { confCapacity = c }

setConnectionSettings :: Connection.Settings -> Config -> Config
setConnectionSettings s config = config { confFetchConnectionSettings = pure s }

setFetchConnectionSettings :: IO Connection.Settings -> Config -> Config
setFetchConnectionSettings s config = config { confFetchConnectionSettings = s }

setAcquisitionTimeout :: Maybe Int -> Config -> Config
setAcquisitionTimeout t config = config { confAcquisitionTimeout = t }

setMaxLifetime :: Maybe Int -> Config -> Config
setMaxLifetime t config = config { confMaxLifetime = t }

setManageInterval :: Int -> Config -> Config
setManageInterval t config = config { confManageInterval = t }

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

withManagedPool :: Config -> (Pool -> IO a) -> IO a
withManagedPool config inner =
  bracket
    (acquireConf config)
    release
    (\pool -> withAsync (manage pool) (const $ inner pool))

acquireConf :: Config -> IO Pool
acquireConf config =
  Pool config
    <$> newTQueueIO
    <*> newTVarIO (confCapacity config)
    <*> (newTVarIO =<< newTVarIO True)

-- | Create a connection-pool.
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
-- In difference to 'acquire' new settings get fetched each time a connection
-- is created. This may be useful for some security models.
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
  acquireConf .
    setCapacity poolSize .
    setFetchConnectionSettings fetchConnectionSettings .
    setAcquisitionTimeout acqTimeout $
    defaultConfig

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

manage :: Pool -> IO ()
manage pool@Pool {..} = forever $ do
  threadDelay (confManageInterval poolConfig)
  clean pool

clean :: Pool -> IO ()
clean Pool {..} = do
  now <- getMonotonicTimeNSec
  join . atomically $ do
    conns <- flushTQueue poolConnectionQueue
    let (keep, close) = partition (isAlive now) conns
    traverse_ (writeTQueue poolConnectionQueue) keep
    return $ forM_ close $ \conn -> do
      Connection.release (connConnection conn)
      atomically $ modifyTVar' poolCapacity succ
  where
    isAlive now conn = case confMaxLifetime poolConfig of
      Nothing -> True
      Just lifetimeUSec -> now <= connCreationTimeNSec conn + 1000 * fromIntegral lifetimeUSec

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
      Just lifetimeUSec -> do
        now <- getMonotonicTimeNSec
        if now > connCreationTimeNSec conn + 1000 * fromIntegral lifetimeUSec
          then do
            Connection.release (connConnection conn)
            onNewConn reuseVar
          else onLiveConn reuseVar conn

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
