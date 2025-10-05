module Specs.BySubject.ReleaseSpec where

import Hasql.Pool
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "The pool remains usable after release" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ selectOneSession
      release pool
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
