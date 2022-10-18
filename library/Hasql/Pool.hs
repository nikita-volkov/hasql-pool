module Hasql.Pool
  ( -- * Pool
    Pool,
    acquire,
    acquireDynamically,
    release,
    use,

    -- * Errors
    UsageError (..),
  )
where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import Hasql.Session (Session)
import qualified Hasql.Session as Session

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Connection settings.
    poolFetchConnectionSettings :: IO Connection.Settings,
    -- | Acquisition timeout, in microseconds.
    poolAcquisitionTimeout :: Maybe Int,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Connection,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Whether to return a connection to the pool.
    poolReuse :: TVar (TVar Bool)
  }

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
acquire poolSize timeout connectionSettings =
  acquireDynamically poolSize timeout (pure connectionSettings)

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
acquireDynamically poolSize timeout fetchConnectionSettings = do
  Pool fetchConnectionSettings timeout
    <$> newTQueueIO
    <*> newTVarIO poolSize
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
    modifyTVar' poolCapacity (+ (length conns))
    return $ forM_ conns Connection.release

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess = do
  timeout <- case poolAcquisitionTimeout of
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
      settings <- poolFetchConnectionSettings
      connRes <- Connection.acquire settings
      case connRes of
        Left connErr -> do
          atomically $ modifyTVar' poolCapacity succ
          return $ Left $ ConnectionUsageError connErr
        Right conn -> onConn reuseVar conn
    onConn reuseVar conn = do
      sessRes <-
        Session.run sess conn
          `catch`
            (\(err :: SomeException ) -> do
              atomically $ modifyTVar' poolCapacity succ
              throw err)
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
              else do
                modifyTVar' poolCapacity succ
                return $ Connection.release conn

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
