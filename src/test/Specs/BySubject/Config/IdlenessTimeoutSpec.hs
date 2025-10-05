module Specs.BySubject.Config.IdlenessTimeoutSpec where

import Hasql.Pool
import Helpers.Sessions
import Scripts qualified
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Times out old connections (maxIdletime)" \scopeParams -> do
    -- 0.5s connection idle time
    Scripts.onAutotaggedPool 1 10 1_800 0.5 scopeParams \_ pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
      -- busy sleep, to keep connection alive
      forM_ [1 :: Int .. 10] $ \_ -> do
        r <- use pool $ selectOneSession
        r `shouldBe` Right 1
        threadDelay 100_000 -- 0.1s
      res3 <- use pool $ getSettingSession "testing.foo"
      res3 `shouldBe` Right (Just "hello world")
      -- idle sleep, connection times out
      threadDelay 1_000_000 -- 1s
      res4 <- use pool $ getSettingSession "testing.foo"
      res4 `shouldBe` Right Nothing
