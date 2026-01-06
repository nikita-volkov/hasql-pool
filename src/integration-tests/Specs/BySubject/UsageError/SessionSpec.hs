module Specs.BySubject.UsageError.SessionSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Bad SQL query triggers error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool Sessions.badQuery
      shouldSatisfy res $ \case
        Left (SessionUsageError _) -> True
        _ -> False
