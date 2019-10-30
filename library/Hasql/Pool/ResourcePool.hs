{-|
Extras for the resource-pool library.
-}
module Hasql.Pool.ResourcePool
where

import Hasql.Pool.Prelude
import Data.Pool


withResourceOnEither :: Pool resource -> (qfailure -> Bool) -> (resource -> IO (Either cfailure (Either qfailure success))) -> IO (Either cfailure (Either qfailure success))
withResourceOnEither pool check act = mask_ $ do
  (resource, localPool) <- takeResource pool
  failureOrSuccess <- act resource `onException` destroyResource pool localPool resource
  case failureOrSuccess of
    r@(Right (Right success)) -> do
      putResource localPool resource
      return r
    r@(Right (Left failure)) | check failure -> do
      putResource localPool resource
      return r
    r@(Right (Left failure)) | otherwise -> do
      destroyResource pool localPool resource
      return r
    l@(Left failure) -> do
      destroyResource pool localPool resource
      return l
