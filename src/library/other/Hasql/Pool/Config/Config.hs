module Hasql.Pool.Config.Config where

import Hasql.Connection qualified as Connection
import Hasql.Pool.Config.Defaults qualified as Defaults
import Hasql.Pool.Observation (Observation)
import Hasql.Pool.Prelude
import Hasql.Session qualified as Session

-- | Configufation for Hasql connection pool.
data Config = Config
  { size :: Int,
    acquisitionTimeout :: DiffTime,
    agingTimeout :: DiffTime,
    idlenessTimeout :: DiffTime,
    connectionSettingsProvider :: IO Connection.Settings,
    observationHandler :: Observation -> IO (),
    initSession :: Session.Session ()
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
      observationHandler = Defaults.observationHandler,
      initSession = Defaults.initSession
    }
