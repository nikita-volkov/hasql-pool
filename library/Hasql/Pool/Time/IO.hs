module Hasql.Pool.Time.IO where

import Hasql.Pool.Prelude
import Hasql.Pool.Time.Conversions

getMillisecondsSinceEpoch :: IO Int
getMillisecondsSinceEpoch =
  fmap toMilliseconds getSystemTime
