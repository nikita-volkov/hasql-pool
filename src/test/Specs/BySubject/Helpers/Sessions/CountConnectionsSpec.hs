module Specs.BySubject.Helpers.Sessions.CountConnectionsSpec where

import Hasql.Pool
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Counts active connections" \scopeParams -> do
    Scripts.onAutotaggedPool 3 10 1_800 1_800 scopeParams \appName pool -> do
      res <- use pool $ countConnectionsSession appName
      res `shouldBe` Right 1
