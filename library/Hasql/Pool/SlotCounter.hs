module Hasql.Pool.SlotCounter
  ( SlotCounter,
    new,
    dec,
    inc,
  )
where

import Hasql.Pool.Prelude

data SlotCounter
  = SlotCounter
      Int
      -- ^ Slots in total.
      (TVar Int)
      -- ^ Slots available for establishing new connections.

new :: Int -> STM SlotCounter
new total =
  SlotCounter total <$> newTVar total

-- | Signals whether we're starting fresh.
dec :: SlotCounter -> STM Bool
dec (SlotCounter slotsInTotal slotsAvailVar) = do
  slotsAvail <- readTVar slotsAvailVar
  if slotsAvail <= 0
    then retry
    else do
      writeTVar slotsAvailVar $! pred slotsAvail
      return $ slotsAvail == slotsInTotal

-- | Signals whether we were full.
inc :: SlotCounter -> STM Bool
inc (SlotCounter slotsInTotal slotsAvailVar) = do
  slotsAvail <- readTVar slotsAvailVar
  if slotsAvail == slotsInTotal
    then retry
    else do
      writeTVar slotsAvailVar $! succ slotsAvail
      return $ slotsAvail == 0
