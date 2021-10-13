{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasql.Pool
(
  Pool,
  Settings(..),
  ConnectionAction(..),
  defaultSettings,
  defaultOnQueryError,
  acquire,
  release,
  UsageError(..),
  use,
)
where

import Hasql.Pool.Prelude
import qualified Hasql.Connection
import qualified Hasql.Session
import Hasql.Session (QueryError(..), CommandError(ClientError))
import qualified Data.Pool as ResourcePool
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Hasql

-- |
-- A pool of connections to DB.
data Pool
  = Pool
  { pool :: ResourcePool.Pool (Either Hasql.Connection.ConnectionError Hasql.Connection.Connection)
  , poolOnQueryError :: QueryError -> IO ConnectionAction
  }

instance Show Pool where
  -- Just display the pool.
  showsPrec i (Pool { pool }) = showsPrec i pool


-- | Settings of the connection pool. Consist of:
data Settings
  = Settings
  { poolSize :: Int
  -- ^ size of the pool
  , timeout :: NominalDiffTime
  -- ^ An amount of time for which an unused resource is kept open. The smallest acceptable value is 0.5 seconds.
  , connectionSettings :: Hasql.Connection.Settings
  -- ^ Connection settings.
  , onQueryError :: QueryError -> IO ConnectionAction
  -- ^ Callback to be run whenver a query returns an error, to determine whether the connection is still healthy.
  -- This is allowed to do I/O, so that more complicated logic can be implemented (e.g. via @Database.PQ.reset@ from @libpq@) if required.
  --
  -- See 'defaultOnQueryError' for the default logic.
}

-- | Return from the the 'onQueryError' to tell the pool whether to drop the connection.
data ConnectionAction
  = KeepConnection
  -- ^ It was determined that the 'Connection' is still good, keep it.
  | DropConnection
  -- ^ The 'Connection' should be dropped.

instance Show Settings where
  show (Settings { poolSize, timeout, connectionSettings }) = "Settings { poolSize = " <> show poolSize <> ", timeout = " <> show timeout <> ", connectionSettings = " <> show connectionSettings <> " }"

-- | Sets `onQueryError` to silently drop the connection if a 'ClientError' is encountered by a query,
-- since that prevents us from re-using stale connections during multiple queries.
defaultOnQueryError :: QueryError -> IO ConnectionAction
defaultOnQueryError (QueryError _ _ (ClientError err)) = pure DropConnection
defaultOnQueryError _ = pure KeepConnection

defaultSettings :: Settings
defaultSettings
  = Settings
  { poolSize = 5
  , timeout = 60.0 :: NominalDiffTime
  , connectionSettings = ""
  , onQueryError = defaultOnQueryError
  }

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (Settings { poolSize, timeout, connectionSettings, onQueryError }) = do
  pool <- ResourcePool.createPool acquire release stripes timeout poolSize
  pure $ Pool {
    poolOnQueryError = onQueryError,
    ..
  }
  where
    acquire =
      Hasql.Connection.acquire connectionSettings
    release =
      either (const (pure ())) Hasql.Connection.release
    stripes =
      1

-- |
-- Release the connection-pool.
release :: Pool -> IO ()
release (Pool { pool }) =
  ResourcePool.destroyAllResources pool

-- |
-- A union over the connection establishment error and the session error.
data UsageError =
  ConnectionError Hasql.ConnectionError |
  SessionError Hasql.Session.QueryError
  deriving (Show, Eq)

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use
  :: Pool
  -> Hasql.Session.Session a
  -> IO (Either UsageError a)
use (Pool { pool, poolOnQueryError }) session =
  -- mask the code, so that async exceptions don’t interrupt the `takeResource`
  mask_ $ do

    -- Take the connection from the pool
    (eConn :: Either Hasql.ConnectionError Connection, localPool) <- ResourcePool.takeResource pool

    -- the connection can’t be used again, destroy it
    let destroyConn = ResourcePool.destroyResource pool localPool eConn
    -- the connection should be put back in the pool
    let keepConn = ResourcePool.putResource localPool eConn

    case eConn of
      -- on connection error, we destroy the connection and return the connection error
      -- (it happened when the connection was initially created)
      Left connErr -> do
        destroyConn
        pure $ Left $ ConnectionError connErr
      Right conn -> do
        res <- Hasql.Session.run session conn
                -- before the exception is rethrown, make sure the connection is destroyed
                `onException` destroyConn
        case res of
          Right a -> do
            keepConn
            pure $ Right a

          Left queryErr -> do
            -- Run the user-defined action if a query error is encountered,
            -- then use the returned ConnectionAction to determine whether
            -- the connection should be dropped.
            connAction <- poolOnQueryError queryErr `onException` destroyConn
            case connAction of
              KeepConnection -> do
                keepConn
                pure $ Left $ SessionError queryErr
              DropConnection -> do
                destroyConn
                pure $ Left $ SessionError queryErr
