module Hasql.PoolSpec
  ( spec
  ) where

import           Test.Hspec

import qualified Data.Pool
import qualified Hasql.Connection
import qualified Hasql.Decoders   as Decoders
import qualified Hasql.Encoders   as Encoders
import           Hasql.Pool
import           Hasql.Prelude
import           Hasql.Session
import           Hasql.Statement

testPool :: IORef ResourceEvent -> IO Pool
testPool ioRef =
  Pool <$>
  Data.Pool.createPool
    onCreateResource
    onDestroyResource
    stripes
    seconds
    poolSize
  where
    onCreateResource =
      Hasql.Connection.acquire -- make an actual connection attempt resulting in a `Left ConnectionError`
        "host=unresolvable-address port=5432 dbname=mydb connect_timeout=1"
    onDestroyResource = const (atomicWriteIORef ioRef Destroyed)
    stripes = 1
    seconds = 2
    poolSize = 1

data ResourceEvent
  = Created
  | Destroyed
  deriving (Show, Eq)

spec :: Spec
spec =
  describe "Hasql.Pool.use" $ do
    it
      "releases a connection back into the pool when resource in pool is a Left" $ do
      ioRef <- (newIORef Created :: IO (IORef ResourceEvent)) -- a resource in the pool to observe on
      p <- testPool ioRef -- put it in a Hasql.Pool.Pool
      _ <- use p testSession -- trigger use
      readIORef ioRef `shouldReturn` Destroyed -- expectation
  where
    testStatement = Statement "" Encoders.unit Decoders.unit True
    testSession = statement () testStatement
