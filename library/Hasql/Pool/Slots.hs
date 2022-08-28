module Hasql.Pool.Slots
  ( Slots,
    new,
    FetchResult (..),
    fetch,
    allocate,
    release,
  )
where

import qualified Hasql.Pool.BoundedCounter as BoundedCounter
import Hasql.Pool.Prelude

data Slots slot
  = Slots
      (TQueue slot)
      BoundedCounter.BoundedCounter

new :: Int -> STM (Slots a)
new size =
  Slots <$> newTQueue <*> BoundedCounter.new size

-- | Result of 'fetch'.
data FetchResult slot
  = AllocatedSlotFetchResult slot
  | VacantFetchResult
  | VacantAndEmptyFetchResult

fetch :: Slots a -> STM (FetchResult a)
fetch (Slots queue counter) =
  AllocatedSlotFetchResult <$> readTQueue queue
    <|> bool VacantFetchResult VacantAndEmptyFetchResult <$> BoundedCounter.inc counter

allocate :: Slots a -> a -> STM ()
allocate (Slots queue _) slot =
  writeTQueue queue slot

release :: Slots a -> STM Bool
release (Slots _ counter) =
  BoundedCounter.dec counter
