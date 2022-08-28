module Hasql.Pool.EntryQueue where

import Hasql.Pool.Prelude
import qualified Hasql.Pool.SlotCounter as SlotCounter

data EntryQueue entry
  = EntryQueue
      (TQueue entry)
      -- ^ Queue of established connections.
      SlotCounter.SlotCounter

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
      fresh <- SlotCounter.dec counter
      return $
        if fresh
          then VacantAndEmptyFetchResult
          else VacantFetchResult

registerEntry :: EntryQueue a -> a -> STM ()
registerEntry (EntryQueue queue _) entry =
  writeTQueue queue entry

releaseSlot :: EntryQueue a -> STM Bool
releaseSlot (EntryQueue _ counter) =
  SlotCounter.inc counter
