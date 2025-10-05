module Specs.BySubject.Helpers.Sessions.GetSettingSpec where

import Hasql.Pool
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Getting and setting session variables works" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
      res <- use pool $ do
        setSettingSession "testing.foo" "hello world"
        getSettingSession "testing.foo"
      res `shouldBe` Right (Just "hello world")

  it "Session variables stay set when a connection gets reused" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")

  it "Releasing the pool resets session variables" \scopeParams -> do
    varName <- Scripts.generateName "var-"
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool $ setSettingSession varName "hello world"
      res `shouldBe` Right ()
      release pool
      res <- use pool $ getSettingSession varName
      res `shouldBe` Right Nothing
