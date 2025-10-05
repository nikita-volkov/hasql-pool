module Specs.BySubject.ReleaseSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "The pool remains usable after release" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ Sessions.selectOne
      release pool
      res <- use pool $ Sessions.selectOne
      shouldSatisfy res $ isRight
