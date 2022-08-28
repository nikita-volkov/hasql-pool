module Hasql.Pool.Slots
  ( Slots,
    new,
    FetchResult (..),
    fetch,
    fetchAll,
    occupy,
    release,
    makeAvail,
  )
where

import qualified BoundedCounter as BoundedCounter
import PowerPrelude

data Slots slot
  = Slots
      (TQueue slot)
      BoundedCounter.BoundedCounter

new :: Int -> STM (Slots a)
new size =
  Slots <$> newTQueue <*> BoundedCounter.new size

-- | Result of 'fetch'.
data FetchResult slot
  = RegisteredSlotFetchResult slot
  | VacantFetchResult
  | VacantAndEmptyFetchResult

occupy :: Slots a -> a -> STM ()
occupy (Slots queue _) slot =
  writeTQueue queue slot

fetch :: Slots a -> STM (FetchResult a)
fetch (Slots queue counter) =
  RegisteredSlotFetchResult <$> readTQueue queue
    <|> bool VacantFetchResult VacantAndEmptyFetchResult <$> BoundedCounter.inc counter

fetchAll :: Slots a -> STM [a]
fetchAll (Slots queue counter) =
  error "TODO"

release :: Slots a -> a -> STM ()
release (Slots queue _) slot =
  unGetTQueue queue slot

makeAvail :: Slots a -> STM Bool
makeAvail (Slots _ counter) =
  BoundedCounter.dec counter
