module GenericPool
  ( Pool,
    acquire,
    release,
    use,
  )
where

import qualified BoundedCounter
import PowerPrelude
import qualified TimeExtras.IO as TimeExtrasIO

-- | Pool of connections to DB.
data Pool acqErr handle = Pool
  { -- | Connection poolTimeout.
    poolTimeout :: Int,
    poolCreator :: IO (Either acqErr handle),
    poolDestroyer :: handle -> IO (),
    -- | Queue of established connections.
    poolSlotsQueue :: TQueue (Slot handle),
    poolSlotsTakenCounter :: BoundedCounter.BoundedCounter,
    -- | Timestamp of the last release. Checked by connections in use to determine, whether they should be released.
    poolReleaseVar :: TVar Int
  }

-- | Available handle.
data Slot handle = Slot
  { slotLastUseTimestamp :: Int,
    slotHandle :: handle
  }

acquire :: Int -> Int -> IO (Either acqErr handle) -> (handle -> IO ()) -> IO (Pool acqErr handle)
acquire timeout size creator destroyer =
  atomically $
    Pool timeout creator destroyer
      <$> newTQueue
      <*> BoundedCounter.create size
      <*> newTVar 0

-- |
-- Release all connections in the pool.
-- Connections currently in use will get released right after the use.
release :: Pool acqErr handle -> IO ()
release Pool {..} = do
  ts <- TimeExtrasIO.getMillisecondsSinceEpoch
  activeSlots <- atomically $ do
    writeTVar poolReleaseVar ts
    flushTQueue poolSlotsQueue
  forM_ activeSlots $ \Slot {..} -> do
    atomically $ BoundedCounter.dec poolSlotsTakenCounter
    poolDestroyer slotHandle

use :: Pool acqErr handle -> (handle -> IO (res, Bool)) -> IO (Either acqErr res)
use Pool {..} use =
  join . atomically $
    tryReadTQueue poolSlotsQueue >>= \case
      Nothing -> do
        first <- BoundedCounter.inc poolSlotsTakenCounter
        return $ do
          res <- acquireThenUseThenRegister
          when first . void . forkIO $ collectGarbage
          return res
      Just Slot {..} ->
        return $ useThenRegister slotLastUseTimestamp slotHandle
  where
    acquireThenUseThenRegister =
      poolCreator >>= \case
        -- Failed to acquire, so release an availability slot,
        -- returning the error details.
        Left acquisitionError -> do
          atomically $ BoundedCounter.dec poolSlotsTakenCounter
          return (Left acquisitionError)
        Right handle -> do
          ts <- TimeExtrasIO.getMillisecondsSinceEpoch
          useThenRegister ts handle

    useThenRegister lastUseTs handle =
      do
        (res, drop) <- use handle
        if drop
          then close handle
          else register lastUseTs handle
        return $ Right res

    register lastUseTs handle = do
      lastReleaseTs <- readTVarIO poolReleaseVar
      if lastReleaseTs >= lastUseTs
        then close handle
        else do
          ts <- TimeExtrasIO.getMillisecondsSinceEpoch
          atomically $ writeTQueue poolSlotsQueue (Slot ts handle)

    close handle = do
      atomically $ BoundedCounter.dec poolSlotsTakenCounter
      poolDestroyer handle

    collectGarbage = do
      ts <- TimeExtrasIO.getMillisecondsSinceEpoch
      let minTs = ts - poolTimeout
      join . atomically $
        tryReadTQueue poolSlotsQueue >>= \case
          Nothing -> do
            first <- BoundedCounter.isEmpty poolSlotsTakenCounter
            if first
              then return $ return ()
              else retry
          Just slot@Slot {..} ->
            if slotLastUseTimestamp <= minTs
              then do
                BoundedCounter.dec poolSlotsTakenCounter
                return $ do
                  poolDestroyer slotHandle
                  collectGarbage
              else do
                unGetTQueue poolSlotsQueue slot
                return $ do
                  TimeExtrasIO.sleepUntilInMilliseconds $ slotLastUseTimestamp + poolTimeout
                  collectGarbage
