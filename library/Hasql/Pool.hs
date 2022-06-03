module Hasql.Pool
  ( -- * --
    Pool,
    acquire,
    release,
    use,
    -- * --
    UsageError (..),
  )
where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import Hasql.Session (Session)
import qualified Hasql.Session as Session

-- |
-- A pool of connections to DB.
data Pool = Pool
  { -- | Connection settings.
    poolConnectionSettings :: Connection.Settings,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Connection,
    -- | Capacity.
    poolCapacity :: TVar Int,
    -- | Alive.
    poolAlive :: TVar Bool
  }

-- | Given the pool-size and connection settings create a connection-pool.
acquire :: Int -> Connection.Settings -> IO Pool
acquire poolSize connectionSettings = do
  Pool connectionSettings
    <$> newTQueueIO
    <*> newTVarIO poolSize
    <*> newTVarIO True

-- |
-- Release all the connections in the pool.
release :: Pool -> IO ()
release Pool {..} = do
  connections <- atomically $ do
    writeTVar poolAlive False
    flushTQueue poolConnectionQueue
  forM_ connections Connection.release

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
-- 
-- Session failing with a 'Session.ClientError' get interpreted as a loss of
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
      connRes <- Connection.acquire poolConnectionSettings
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
        returnConn = do
          atomically $ do
            alive <- readTVar poolAlive
            when alive $ writeTQueue poolConnectionQueue conn

-- |
-- A union over the connection establishment error and the session error.
data UsageError
  = ConnectionUsageError Connection.ConnectionError
  | SessionUsageError Session.QueryError
  | PoolIsReleasedUsageError
  deriving (Show, Eq)

instance Exception UsageError
