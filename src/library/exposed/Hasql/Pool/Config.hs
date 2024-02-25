-- | DSL for construction of configs.
module Hasql.Pool.Config
  ( Config.Config,
    compile,
    Setting.Setting,
    Setting.size,
    Setting.acquisitionTimeout,
    Setting.agingTimeout,
    Setting.idlenessTimeout,
    Setting.staticConnectionString,
    Setting.dynamicConnectionString,
    Setting.observationHandler,
  )
where

import qualified Hasql.Pool.Config.Config as Config
import qualified Hasql.Pool.Config.Setting as Setting
import Hasql.Pool.Prelude

-- | Compile config from a list of settings.
-- Latter settings override the preceding in cases of conflicts.
compile :: [Setting.Setting] -> Config.Config
compile =
  foldr ($) Config.defaults . fmap Setting.apply
