module Specs.BySubject.UseSpec where

import Hasql.Pool
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Releases a spot in the pool when there is a query error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool badQuerySession `shouldNotReturn` (Right ())
      use pool selectOneSession `shouldReturn` (Right 1)

  it "Connection errors cause eviction of connection" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ closeConnSession >> selectOneSession
      _ <- use pool $ closeConnSession >> selectOneSession
      _ <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "Connection gets returned to the pool after normal use" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "Connection gets returned to the pool after non-connection error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
