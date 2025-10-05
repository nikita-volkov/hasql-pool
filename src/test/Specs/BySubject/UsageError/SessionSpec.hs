module Specs.BySubject.UsageError.SessionSpec where

import Hasql.Pool
import Hasql.Session qualified as Session
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Simulation of connection error works" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool $ closeConnSession >> selectOneSession
      shouldSatisfy res $ \case
        Left (SessionUsageError (Session.QueryError _ _ (Session.ClientError _))) -> True
        _ -> False
