module BoundedCounter
  ( BoundedCounter,
    create,
    dec,
    inc,
    isEmpty,
  )
where

import PowerPrelude

data BoundedCounter
  = BoundedCounter
      Int
      -- ^ Maximum.
      (TVar Int)
      -- ^ Counter.

create :: Int -> STM BoundedCounter
create maximum =
  BoundedCounter maximum <$> newTVar 0

dec :: BoundedCounter -> STM Bool
dec (BoundedCounter maximum countVar) = do
  count <- readTVar countVar
  if count <= 0
    then retry
    else do
      writeTVar countVar $! pred count
      return $ count == maximum

inc :: BoundedCounter -> STM Bool
inc (BoundedCounter maximum countVar) = do
  count <- readTVar countVar
  if count == maximum
    then retry
    else do
      writeTVar countVar $! succ count
      return $ count == 0

isEmpty :: BoundedCounter -> STM Bool
isEmpty (BoundedCounter _ countVar) = do
  count <- readTVar countVar
  return $ count == 0
