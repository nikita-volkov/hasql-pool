module Hasql.Pool
  ( Pool,
    Settings (..),
    acquire,
    release,
    UsageError (..),
    use,
  )
where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import Hasql.Pool.Slots (Slots)
import qualified Hasql.Pool.Slots as Slots
import qualified Hasql.Pool.TimeExtras.IO as TimeExtrasIO
import Hasql.Session (Session)
import qualified Hasql.Session as Session

-- |
-- Pool of connections to DB.
data Pool = Pool
  { -- | Connection timeout.
    poolTimeout :: Int,
    -- | Queue of established connections.
    poolConnectionSettings :: Connection.Settings,
    -- | Connection settings.
    poolSlots :: Slots ActiveSlot,
    -- | Timestamp of the last release. Checked by connections in use to determine, whether they should be released.
    poolReleaseTimestamp :: TVar Int
  }

-- | Available connection.
data ActiveSlot = ActiveSlot
  { activeSlotLastUseTimestamp :: Int,
    activeSlotConnection :: Connection
  }

-- |
-- Settings of the connection pool. Consist of:
--
-- * Pool-size.
--
-- * Timeout.
-- An amount of time in milliseconds for which the unused connections are kept open.
--
-- * Connection settings.
type Settings =
  (Int, Int, Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire (size, timeout, connectionSettings) =
  atomically $ Pool timeout connectionSettings <$> Slots.new size <*> newTVar 0

-- |
-- Release all connections in the pool.
-- Connections currently in use will get released right after the use.
release :: Pool -> IO ()
release (Pool _ _ slots lastReleaseVar) = do
  ts <- TimeExtrasIO.getMillisecondsSinceEpoch
  activeSlots <- atomically $ do
    writeTVar lastReleaseVar ts
    Slots.fetchAll slots
  forM_ activeSlots $ Connection.release . activeSlotConnection

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
use (Pool timeout connectionSettings slots lastReleaseVar) session =
  join . atomically $
    Slots.fetch slots >>= \case
      Slots.RegisteredSlotFetchResult (ActiveSlot lastUseTs connection) ->
        return $ useConnectionThenRegister lastUseTs connection
      Slots.VacantAndEmptyFetchResult ->
        return $ do
          res <- acquireConnectionThenUseThenRegister session
          void $ forkIO collectGarbage
          return res
      Slots.VacantFetchResult ->
        return $ acquireConnectionThenUseThenRegister session
  where
    acquireConnectionThenUseThenRegister session =
      Connection.acquire connectionSettings >>= \case
        -- Failed to acquire, so release an availability slot,
        -- returning the error details.
        Left acquisitionError -> do
          atomically $ Slots.makeAvail slots
          return (Left (ConnectionUsageError acquisitionError))
        Right connection -> do
          ts <- TimeExtrasIO.getMillisecondsSinceEpoch
          useConnectionThenRegister ts connection

    useConnectionThenRegister lastUseTs connection =
      Session.run session connection >>= \case
        Left queryError -> do
          -- Check whether the error is on client-side,
          -- and in that case release the connection.
          case queryError of
            Session.QueryError _ _ (Session.ClientError _) ->
              closeConnection connection
            _ ->
              registerConnection lastUseTs connection
          return (Left (SessionUsageError queryError))
        Right res -> do
          registerConnection lastUseTs connection
          return (Right res)

    registerConnection lastUseTs connection = do
      lastReleaseTs <- readTVarIO lastReleaseVar
      if lastReleaseTs >= lastUseTs
        then closeConnection connection
        else do
          ts <- TimeExtrasIO.getMillisecondsSinceEpoch
          atomically $ Slots.occupy slots (ActiveSlot ts connection)

    closeConnection connection = do
      atomically $ Slots.makeAvail slots
      Connection.release connection

    collectGarbage = do
      ts <- TimeExtrasIO.getMillisecondsSinceEpoch
      let minTs = ts - timeout
      join . atomically $ do
        Slots.fetch slots >>= \case
          Slots.RegisteredSlotFetchResult (ActiveSlot lastUseTs connection) ->
            if lastUseTs <= minTs
              then do
                Slots.makeAvail slots
                return $ do
                  Connection.release connection
                  collectGarbage
              else do
                Slots.release slots $ ActiveSlot lastUseTs connection
                return $ do
                  TimeExtrasIO.sleepUntilInMilliseconds $ lastUseTs + timeout
                  collectGarbage
          Slots.VacantFetchResult -> retry
          Slots.VacantAndEmptyFetchResult -> do
            Slots.makeAvail slots
            return $ return ()
