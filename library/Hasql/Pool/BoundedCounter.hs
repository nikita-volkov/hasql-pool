module Hasql.Pool.BoundedCounter
  ( BoundedCounter,
    new,
    dec,
    inc,
  )
where

import Hasql.Pool.Prelude

data BoundedCounter
  = BoundedCounter
      Int
      -- ^ Slots in total.
      (TVar Int)
      -- ^ Slots available for establishing new connections.

new :: Int -> STM BoundedCounter
new total =
  BoundedCounter total <$> newTVar total

-- | Signals whether we're starting fresh.
dec :: BoundedCounter -> STM Bool
dec (BoundedCounter slotsInTotal slotsAvailVar) = do
  slotsAvail <- readTVar slotsAvailVar
  if slotsAvail <= 0
    then retry
    else do
      writeTVar slotsAvailVar $! pred slotsAvail
      return $ slotsAvail == slotsInTotal

-- | Signals whether we were full.
inc :: BoundedCounter -> STM Bool
inc (BoundedCounter slotsInTotal slotsAvailVar) = do
  slotsAvail <- readTVar slotsAvailVar
  if slotsAvail == slotsInTotal
    then retry
    else do
      writeTVar slotsAvailVar $! succ slotsAvail
      return $ slotsAvail == 0
