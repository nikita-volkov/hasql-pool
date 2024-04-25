-- | DSL for construction of configs.
module Hasql.Pool.Config
  ( Config.Config,
    settings,
    Setting.Setting,
    Setting.size,
    Setting.acquisitionTimeout,
    Setting.agingTimeout,
    Setting.idlenessTimeout,
    Setting.staticConnectionSettings,
    Setting.dynamicConnectionSettings,
    Setting.observationHandler,
    Setting.initSession,
  )
where

import qualified Hasql.Pool.Config.Config as Config
import qualified Hasql.Pool.Config.Setting as Setting
import Hasql.Pool.Prelude

-- | Compile config from a list of settings.
-- Latter settings override the preceding in cases of conflicts.
settings :: [Setting.Setting] -> Config.Config
settings =
  foldr ($) Config.defaults . fmap Setting.apply
