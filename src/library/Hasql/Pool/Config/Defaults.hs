module Hasql.Pool.Config.Defaults where

import Hasql.Connection.Settings qualified as Connection.Settings
import Hasql.Pool.Observation (Observation)
import Hasql.Pool.Prelude
import Hasql.Session qualified as Session

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
staticConnectionSettings :: Connection.Settings.Settings
staticConnectionSettings =
  "postgresql://postgres:postgres@localhost:5432/postgres"

-- |
-- > pure "postgresql://postgres:postgres@localhost:5432/postgres"
dynamicConnectionSettings :: IO Connection.Settings.Settings
dynamicConnectionSettings = pure staticConnectionSettings

-- |
-- > const (pure ())
observationHandler :: Observation -> IO ()
observationHandler = const (pure ())

-- |
-- > pure ()
initSession :: Session.Session ()
initSession = pure ()
