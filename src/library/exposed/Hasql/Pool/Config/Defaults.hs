module Hasql.Pool.Config.Defaults where

import qualified Hasql.Connection as Connection
import Hasql.Pool.Observation (Observation)
import Hasql.Pool.Prelude
import qualified Hasql.Session as Session

-- |
-- 3 connections.
size :: Int
size = 3

-- |
-- 10 seconds.
acquisitionTimeout :: DiffTime
acquisitionTimeout = 10

-- |
-- 1 day.
agingTimeout :: DiffTime
agingTimeout = 60 * 60 * 24

-- |
-- 10 minutes.
idlenessTimeout :: DiffTime
idlenessTimeout = 60 * 10

-- |
-- > "postgresql://postgres:postgres@localhost:5432/postgres"
staticConnectionSettings :: Connection.Settings
staticConnectionSettings = "postgresql://postgres:postgres@localhost:5432/postgres"

-- |
-- > pure "postgresql://postgres:postgres@localhost:5432/postgres"
dynamicConnectionSettings :: IO Connection.Settings
dynamicConnectionSettings = pure "postgresql://postgres:postgres@localhost:5432/postgres"

-- |
-- > const (pure ())
observationHandler :: Observation -> IO ()
observationHandler = const (pure ())

-- |
-- > pure ()
initSession :: Session.Session ()
initSession = pure ()
