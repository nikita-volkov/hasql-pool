module TimeExtras.IO where

import PowerPrelude
import TimeExtras.Conversions

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
