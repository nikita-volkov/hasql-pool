module Hasql.Pool.Config.Config where

import qualified Hasql.Connection as Connection
import Hasql.Pool.Observation (Observation)
import Hasql.Pool.Prelude

-- | Configufation for Hasql connection pool.
data Config = Config
  { size :: Int,
    acquisitionTimeout :: DiffTime,
    agingTimeout :: DiffTime,
    idlenessTimeout :: DiffTime,
    connectionSettingsProvider :: IO Connection.Settings,
    observationHandler :: Observation -> IO ()
  }

-- | Reasonable defaults, which can be built upon.
defaults :: Config
defaults =
  Config
    { size = 3,
      acquisitionTimeout = 10,
      agingTimeout = 60 * 60 * 24,
      idlenessTimeout = 60 * 10,
      connectionSettingsProvider = pure "postgresql://postgres:postgres@localhost:5432/postgres",
      observationHandler = const (pure ())
    }
