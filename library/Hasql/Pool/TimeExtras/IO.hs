module Hasql.Pool.TimeExtras.IO where

import Hasql.Pool.Prelude
import Hasql.Pool.TimeExtras.Conversions

getMillisecondsSinceEpoch :: IO Int
getMillisecondsSinceEpoch =
  fmap toMilliseconds getSystemTime

sleepUntilInMilliseconds :: Int -> IO ()
sleepUntilInMilliseconds untilTs = do
  nowTs <- getMillisecondsSinceEpoch
  let diff = untilTs - nowTs
  if diff > 0
    then threadDelay (diff * 1000)
    else return ()
