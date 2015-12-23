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
import qualified Data.Pool


-- |
-- A pool of connections to DB.
newtype Pool =
  Pool (Data.Pool.Pool (Either Hasql.Connection.ConnectionError Hasql.Connection.Connection))

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Int -> NominalDiffTime -> Hasql.Connection.Settings -> IO Pool
acquire size timeout connectionSettings =
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
-- Use a connection from the pool and return it to the pool, when finished.
-- Exception-safe.
use :: Pool -> (Hasql.Connection.Connection -> IO a) -> IO (Either Hasql.Connection.ConnectionError a)
use (Pool pool) handler =
  Data.Pool.withResource pool (traverse handler)

