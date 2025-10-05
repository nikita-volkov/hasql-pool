module Specs.BySubject.Config.AgingTimeoutSpec where

import Hasql.Pool
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Actively times out old connections" \scopeParams -> do
    Scripts.onAutotaggedPool 3 10 1_800 1_800 scopeParams \_appName1 pool1 -> do
      Scripts.onAutotaggedPool 3 10 0.5 1_800 scopeParams \appName2 pool2 -> do
        res <- use pool2 $ Sessions.selectOne
        res `shouldBe` Right 1
        res2 <- use pool1 $ Sessions.countConnections appName2
        res2 `shouldBe` Right 1
        threadDelay 1_000_000 -- 1s
        res3 <- use pool1 $ Sessions.countConnections appName2
        res3 `shouldBe` Right 0

  it "Passively times out old connections" \scopeParams -> do
    -- 0.5s connection lifetime
    Scripts.onAutotaggedPool 1 10 0.5 1_800 scopeParams \_ pool -> do
      varName <- Scripts.generateName "var-"
      res <- use pool $ Sessions.setSetting varName "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ Sessions.getSetting varName
      res2 `shouldBe` Right (Just "hello world")
      threadDelay 1_000_000 -- 1s
      res3 <- use pool $ Sessions.getSetting varName
      res3 `shouldBe` Right Nothing
