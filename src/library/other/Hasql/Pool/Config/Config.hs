module Hasql.Pool.Config.Config where

import qualified Hasql.Connection as Connection
import qualified Hasql.Pool.Config.Defaults as Defaults
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
    { size = Defaults.size,
      acquisitionTimeout = Defaults.acquisitionTimeout,
      agingTimeout = Defaults.agingTimeout,
      idlenessTimeout = Defaults.idlenessTimeout,
      connectionSettingsProvider = Defaults.dynamicConnectionSettings,
      observationHandler = Defaults.observationHandler
    }
