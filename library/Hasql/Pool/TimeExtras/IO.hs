module Hasql.Pool.TimeExtras.IO where

import Hasql.Pool.Prelude
import Hasql.Pool.TimeExtras.Conversions

getMillisecondsSinceEpoch :: IO Int
getMillisecondsSinceEpoch =
  fmap toMilliseconds getSystemTime
