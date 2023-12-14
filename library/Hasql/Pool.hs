module Hasql.Pool
  ( -- * Pool
    Pool,
    acquire,
    acquireDynamically,
    use,
    release,

    -- * Errors
    UsageError (..),
  )
where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import qualified Hasql.Session as Session

-- | A connection tagged with metadata.
data Conn = Conn
  { connConnection :: Connection,
    connCreationTimeNSec :: Word64,
    connUseTimeNSec :: Word64
  }

isAlive :: Word64 -> Word64 -> Word64 -> Conn -> Bool
isAlive maxLifetime maxIdletime now Conn {..} =
  now
    <= connCreationTimeNSec
    + maxLifetime
    && now
    <= connUseTimeNSec
    + maxIdletime

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Pool size.
    poolSize :: Int,
    -- | Connection settings.
    poolFetchConnectionSettings :: IO Connection.Settings,
    -- | Acquisition timeout, in microseconds.
    poolAcquisitionTimeout :: Int,
    -- | Maximal connection lifetime, in nanoseconds.
    poolMaxLifetime :: Word64,
    -- | Maximal connection idle time, in nanoseconds.
    poolMaxIdletime :: Word64,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Conn,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Whether to return a connection to the pool.
    poolReuseVar :: TVar (TVar Bool),
    -- | To stop the manager thread via garbage collection.
    poolReaperRef :: IORef ()
  }

-- | Create a connection-pool, with default settings.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
acquire ::
  -- | Pool size.
  Int ->
  -- | Connection acquisition timeout.
  DiffTime ->
  -- | Maximal connection lifetime.
  DiffTime ->
  -- | Maximal connection idle time.
  DiffTime ->
  -- | Connection settings.
  Connection.Settings ->
  IO Pool
acquire poolSize acqTimeout maxLifetime maxIdletime connectionSettings =
  acquireDynamically poolSize acqTimeout maxLifetime maxIdletime (pure connectionSettings)

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
  -- | Connection acquisition timeout.
  DiffTime ->
  -- | Maximal connection lifetime.
  DiffTime ->
  -- | Maximal connection idle time.
  DiffTime ->
  -- | Action fetching connection settings.
  IO Connection.Settings ->
  IO Pool
acquireDynamically poolSize acqTimeout maxLifetime maxIdletime fetchConnectionSettings = do
  connectionQueue <- newTQueueIO
  capVar <- newTVarIO poolSize
  reuseVar <- newTVarIO =<< newTVarIO True
  reaperRef <- newIORef ()

  managerTid <- forkIOWithUnmask $ \unmask -> unmask $ forever $ do
    threadDelay 1000000
    now <- getMonotonicTimeNSec
    join . atomically $ do
      conns <- flushTQueue connectionQueue
      let (keep, close) = partition (isAlive maxLifetimeNanos maxIdletimeNanos now) conns
      traverse_ (writeTQueue connectionQueue) keep
      return $ forM_ close $ \conn -> do
        Connection.release (connConnection conn)
        atomically $ modifyTVar' capVar succ

  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, stop the manager.
    killThread managerTid

  return $ Pool poolSize fetchConnectionSettings acqTimeoutMicros maxLifetimeNanos maxIdletimeNanos connectionQueue capVar reuseVar reaperRef
  where
    acqTimeoutMicros =
      div (fromIntegral (diffTimeToPicoseconds acqTimeout)) 1_000_000
    maxLifetimeNanos =
      div (fromIntegral (diffTimeToPicoseconds maxLifetime)) 1_000
    maxIdletimeNanos =
      div (fromIntegral (diffTimeToPicoseconds maxIdletime)) 1_000

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
    conns <- flushTQueue poolConnectionQueue
    return $ forM_ conns $ \conn -> do
      Connection.release (connConnection conn)
      atomically $ modifyTVar' poolCapacity succ

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
--
-- __Warning:__ Due to the mechanism mentioned above you should avoid consuming
-- errors within sessions.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess = do
  timeout <- do
    delay <- registerDelay poolAcquisitionTimeout
    return $ readTVar delay
  join . atomically $ do
    reuseVar <- readTVar poolReuseVar
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
      settings <- poolFetchConnectionSettings
      now <- getMonotonicTimeNSec
      connRes <- Connection.acquire settings
      case connRes of
        Left connErr -> do
          atomically $ modifyTVar' poolCapacity succ
          return $ Left $ ConnectionUsageError connErr
        Right conn -> onLiveConn reuseVar (Conn conn now now)

    onConn reuseVar conn = do
      now <- getMonotonicTimeNSec
      if isAlive poolMaxLifetime poolMaxIdletime now conn
        then onLiveConn reuseVar conn {connUseTimeNSec = now}
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
