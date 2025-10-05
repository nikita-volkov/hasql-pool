module Specs.BySubject.Helpers.Sessions.GetSettingSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Getting and setting session variables works" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname
      res <- use pool $ Sessions.getSetting varName
      res `shouldBe` Right Nothing
      res <- use pool $ do
        Sessions.setSetting varName "hello world"
        Sessions.getSetting varName
      res `shouldBe` Right (Just "hello world")

  it "Session variables stay set when a connection gets reused" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      varName <- Scripts.generateVarname
      res <- use pool $ Sessions.setSetting varName "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ Sessions.getSetting varName
      res2 `shouldBe` Right (Just "hello world")

  it "Releasing the pool resets session variables" \scopeParams -> do
    varName <- Scripts.generateVarname
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      res <- use pool $ Sessions.setSetting varName "hello world"
      res `shouldBe` Right ()
      release pool
      res <- use pool $ Sessions.getSetting varName
      res `shouldBe` Right Nothing
