module BoundedCounter
  ( BoundedCounter,
    new,
    dec,
    inc,
  )
where

import PowerPrelude

data BoundedCounter
  = BoundedCounter
      Int
      -- ^ Maximum.
      (TVar Int)
      -- ^ Counter.

new :: Int -> STM BoundedCounter
new maximum =
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
