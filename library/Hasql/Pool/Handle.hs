-- |
-- Handle-pattern API.
module Hasql.Pool.Handle
(
  Handle,
  with,
  -- * Usage
  Error(..),
  session,
)
where

import Hasql.Pool.Prelude hiding (Handle)
import qualified Hasql.Pool
import qualified Hasql.Connection
import qualified Hasql.Session


-- |
-- A temporary handle to the pool. 
newtype Handle s =
  Handle (forall a. Hasql.Session.Session a -> IO (Either Error a))

-- |
-- Given the pool-size, timeout and connection settings
-- executes an IO function on a temporary handle,
-- releasing it automatically afterwards.
with :: Int -> NominalDiffTime -> Hasql.Connection.Settings -> (forall s. Handle s -> IO a) -> IO a
with size timeout settings handler =
  acquire >>= \pool -> use pool <* release pool
  where
    acquire =
      Hasql.Pool.acquire size timeout settings
    use pool =
      handler $ Handle $ \session -> 
        fmap (either (Left . Left) (either (Left . Right) Right)) $
        Hasql.Pool.use pool $
        Hasql.Session.run session
    release pool =
      Hasql.Pool.release pool


-- * Usage
-------------------------

-- |
-- Error of executing a session on a connection from the pool.
type Error =
  Either Hasql.Connection.ConnectionError Hasql.Session.Error

-- |
-- Executes a session on a connection from the pool,
-- using the provided temporary handle to it.
session :: Handle s -> Hasql.Session.Session a -> IO (Either Error a)
session (Handle runSession) session =
  runSession session
