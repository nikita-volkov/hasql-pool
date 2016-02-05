module Hasql.Pool
(
  Pool,
  acquire,
  release,
  use,
)
where

import Hasql.Pool.Prelude
import qualified Hasql.Connection
import qualified Hasql.Session
import qualified Data.Pool


-- |
-- A pool of connections to DB.
newtype Pool =
  Pool (Data.Pool.Pool (Either Hasql.Connection.ConnectionError Hasql.Connection.Connection))

type Settings =
  (Int, NominalDiffTime, Hasql.Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (size, timeout, connectionSettings) =
  fmap Pool $
  Data.Pool.createPool acquire release stripes timeout size
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
release (Pool pool) =
  Data.Pool.destroyAllResources pool

-- |
-- Use a connection from the pool to run a session and
-- and return the connection to the pool, when finished.
-- Exception-safe.
use :: Pool -> Hasql.Session.Session a -> IO (Either (Either Hasql.Connection.ConnectionError Hasql.Session.Error) a)
use (Pool pool) session =
  fmap (either (Left . Left) (either (Left . Right) Right)) $
  Data.Pool.withResource pool $
  traverse $ 
  Hasql.Session.run session
