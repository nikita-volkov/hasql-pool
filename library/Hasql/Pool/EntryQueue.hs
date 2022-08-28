module Hasql.Pool.EntryQueue where

import qualified Hasql.Pool.BoundedCounter as BoundedCounter
import Hasql.Pool.Prelude

data EntryQueue entry
  = EntryQueue
      (TQueue entry)
      -- ^ Queue of established connections.
      BoundedCounter.BoundedCounter

-- | Result of 'fetch'.
data FetchResult entry
  = ExistingEntryFetchResult entry
  | VacantFetchResult
  | VacantAndEmptyFetchResult

fetch :: EntryQueue a -> STM (FetchResult a)
fetch (EntryQueue queue counter) =
  tryReadTQueue queue >>= \case
    Just entry ->
      return $ ExistingEntryFetchResult entry
    Nothing -> do
      fresh <- BoundedCounter.dec counter
      return $
        if fresh
          then VacantAndEmptyFetchResult
          else VacantFetchResult

registerEntry :: EntryQueue a -> a -> STM ()
registerEntry (EntryQueue queue _) entry =
  writeTQueue queue entry

releaseSlot :: EntryQueue a -> STM Bool
releaseSlot (EntryQueue _ counter) =
  BoundedCounter.inc counter
