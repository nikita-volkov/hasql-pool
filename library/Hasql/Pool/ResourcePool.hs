{-|
Extras for the resource-pool library.
-}
module Hasql.Pool.ResourcePool
where

import Hasql.Pool.Prelude
import Data.Pool


withResourceOnEither :: Pool resource -> (resource -> IO (Either failure success)) -> IO (Either failure success)
withResourceOnEither pool act = mask_ $ do
  (resource, localPool) <- takeResource pool
  failureOrSuccess <- act resource
  case failureOrSuccess of
    Right success -> do
      putResource localPool resource
      return (Right success)
    Left failure -> do
      destroyResource pool localPool resource
      return (Left failure)
