module Specs.BySubject.UseSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Releases a spot in the pool when there is a query error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool Sessions.badQuery `shouldNotReturn` (Right ())
      use pool Sessions.selectOne `shouldReturn` (Right 1)

  it "Connection gets returned to the pool after normal use" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ Sessions.selectOne
      _ <- use pool $ Sessions.selectOne
      _ <- use pool $ Sessions.selectOne
      _ <- use pool $ Sessions.selectOne
      res <- use pool $ Sessions.selectOne
      shouldSatisfy res $ isRight

  it "Connection gets returned to the pool after non-connection error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ Sessions.badQuery
      _ <- use pool $ Sessions.badQuery
      _ <- use pool $ Sessions.badQuery
      _ <- use pool $ Sessions.badQuery
      res <- use pool $ Sessions.selectOne
      shouldSatisfy res $ isRight
