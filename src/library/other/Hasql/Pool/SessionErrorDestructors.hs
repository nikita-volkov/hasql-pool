module Hasql.Pool.SessionErrorDestructors where

import Hasql.Errors qualified as Errors
import Hasql.Pool.Prelude

reset :: (Text -> x) -> x -> Errors.SessionError -> x
reset onReset onNoReset = \case
  Errors.ConnectionSessionError details -> onReset details
  _ -> onNoReset
