module Specs.BySubject.Config.InitSessionSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Persists after exceptions thrown in session" \scopeParams -> do
    Scripts.onAutotaggedPool 1 10 60 60 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname

      res <- use pool do
        Sessions.setSetting varName "1"
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

      try @SomeException do
        use pool do
          liftIO do
            throwIO (userError "Intentional error for testing")

      res <- use pool do
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

  it "Persists after bad query" \scopeParams -> do
    Scripts.onAutotaggedPool 1 10 60 60 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname

      res <- use pool do
        Sessions.setSetting varName "1"
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))

      use pool do
        Sessions.badQuery

      res <- use pool do
        Sessions.getSetting varName
      shouldBe res (Right (Just "1"))
