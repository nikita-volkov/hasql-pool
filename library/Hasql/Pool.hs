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

-- | A pool of connections to DB.
data Pool = Pool
  { -- | Connection settings.
    poolFetchConnectionSettings :: IO Connection.Settings,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Connection,
    -- | Capacity.
    poolCapacity :: TVar Int,
    -- | Alive.
    poolAlive :: TVar Bool
  }

-- | Given the pool-size and connection settings create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
acquire :: Int -> Connection.Settings -> IO Pool
acquire poolSize connectionSettings =
  acquireDynamically poolSize (pure connectionSettings)

-- | Given the pool-size and connection settings constructor action
-- create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- In difference to 'acquire' new settings get fetched each time a connection
-- is created. This may be useful for some security models.
acquireDynamically :: Int -> IO Connection.Settings -> IO Pool
acquireDynamically poolSize fetchConnectionSettings = do
  Pool fetchConnectionSettings
    <$> newTQueueIO
    <*> newTVarIO poolSize
    <*> newTVarIO True

-- | Release all the connections in the pool.
release :: Pool -> IO ()
release Pool {..} = do
  connections <- atomically $ do
    writeTVar poolAlive False
    flushTQueue poolConnectionQueue
  forM_ connections Connection.release

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess =
  join . atomically $ do
    alive <- readTVar poolAlive
    if alive
      then
        asum
          [ readTQueue poolConnectionQueue <&> onConn,
            do
              capVal <- readTVar poolCapacity
              if capVal > 0
                then do
                  writeTVar poolCapacity $! pred capVal
                  return onNewConn
                else retry
          ]
      else return . return . Left $ PoolIsReleasedUsageError
  where
    onNewConn = do
      settings <- poolFetchConnectionSettings
      connRes <- Connection.acquire settings
      case connRes of
        Left connErr -> do
          atomically $ modifyTVar' poolCapacity succ
          return $ Left $ ConnectionUsageError connErr
        Right conn -> onConn conn
    onConn conn = do
      sessRes <- Session.run sess conn
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
            alive <- readTVar poolAlive
            if alive
              then writeTQueue poolConnectionQueue conn $> return ()
              else return $ Connection.release conn

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Connection.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Session.QueryError
  | -- | Attempt to use a pool, which has already been called 'release' upon.
    PoolIsReleasedUsageError
  deriving (Show, Eq)

instance Exception UsageError
