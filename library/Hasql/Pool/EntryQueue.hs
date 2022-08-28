module Hasql.Pool.EntryQueue where

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Pool.Prelude
import qualified Hasql.Pool.TimeExtras.IO as TimeExtrasIO
import Hasql.Session (Session)
import qualified Hasql.Session as Session

data EntryQueue entry
  = EntryQueue
      Int
      -- ^ Slots in total.
      (TQueue entry)
      -- ^ Queue of established connections.
      (TVar Int)
      -- ^ Slots available for establishing new connections.

-- | Result of 'fetch'.
data FetchResult entry
  = ExistingEntryFetchResult entry
  | VacantFetchResult
  | VacantAndEmptyFetchResult

fetch :: EntryQueue a -> STM (FetchResult a)
fetch (EntryQueue slotsInTotal queue slotsAvailVar) =
  tryReadTQueue queue >>= \case
    Just entry -> do
      return $ ExistingEntryFetchResult entry
    Nothing -> do
      slotsAvail <- readTVar slotsAvailVar
      if slotsAvail <= 0
        then retry
        else do
          writeTVar slotsAvailVar $! pred slotsAvail
          return $
            if slotsAvail == slotsInTotal
              then VacantAndEmptyFetchResult
              else VacantFetchResult

registerEntry :: EntryQueue a -> a -> STM ()
registerEntry (EntryQueue _ queue _) entry =
  writeTQueue queue entry

releaseSlot :: EntryQueue a -> STM ()
releaseSlot (EntryQueue _ _ slotsAvailVar) =
  modifyTVar' slotsAvailVar succ
