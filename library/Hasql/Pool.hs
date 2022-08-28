module Hasql.Pool
  ( Pool,
    Settings (..),
    acquire,
    release,
    UsageError (..),
    use,
  )
where

import qualified GenericPool
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import PowerPrelude

-- |
-- Pool of connections to DB.
newtype Pool = Pool (GenericPool.Pool Connection.ConnectionError Connection.Connection)

-- |  Settings of the connection pool.
data Settings
  = Settings
      Int
      -- ^ Pool-size.
      Int
      -- ^ Timeout.
      -- An amount of time in milliseconds for which the unused connections are kept open.
      Connection.Settings
      -- ^ Connection settings.

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (Settings size timeout connectionSettings) =
  fmap Pool $
    GenericPool.acquire
      size
      timeout
      (Connection.acquire connectionSettings)
      Connection.release

-- |
-- Release all connections in the pool.
-- Connections currently in use will get released right after finishing being used.
--
-- It is ok to use the pool after releasing.
-- In such case this function effectively resets all connections.
release :: Pool -> IO ()
release (Pool pool) =
  GenericPool.release pool

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
use (Pool pool) session =
  fmap repackResult $ GenericPool.use pool handler
  where
    handler connection =
      Session.run session connection <&> \res -> (res, resIsDrop res)
      where
        resIsDrop = \case
          Left (Session.QueryError _ _ (Session.ClientError _)) -> True
          _ -> False
    repackResult =
      either (Left . ConnectionUsageError) (either (Left . SessionUsageError) Right)
