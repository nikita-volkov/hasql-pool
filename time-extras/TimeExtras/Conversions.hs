module TimeExtras.Conversions where

import PowerPrelude

class ToMilliseconds a where
  toMilliseconds :: a -> Int

instance ToMilliseconds SystemTime where
  toMilliseconds (MkSystemTime s ns) =
    fromIntegral s * 1000 + fromIntegral (div ns 1000000)
