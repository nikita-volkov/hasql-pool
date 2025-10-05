module Main where

import Control.Concurrent.Async (race)
import Data.Text qualified as Text
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting.Connection
import Hasql.Connection.Setting.Connection.Param qualified as Connection.Setting.Connection.Param
import Hasql.Pool
import Hasql.Pool.Config qualified as Config
import Hasql.Session qualified as Session
import Helpers.Sessions
import Helpers.Testcontainers qualified as Testcontainers
import System.Random.Stateful qualified as Random
import Test.Hspec
import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = Testcontainers.aroundAllWithConnectionSettings do
  it "Releases a spot in the pool when there is a query error" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      use pool badQuerySession `shouldNotReturn` (Right ())
      use pool selectOneSession `shouldReturn` (Right 1)

  it "Simulation of connection error works" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      res <- use pool $ closeConnSession >> selectOneSession
      shouldSatisfy res $ \case
        Left (SessionUsageError (Session.QueryError _ _ (Session.ClientError _))) -> True
        _ -> False

  it "Connection errors cause eviction of connection" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      _ <- use pool $ closeConnSession >> selectOneSession
      _ <- use pool $ closeConnSession >> selectOneSession
      _ <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "Connection gets returned to the pool after normal use" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      _ <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "Connection gets returned to the pool after non-connection error" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      _ <- use pool $ badQuerySession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "The pool remains usable after release" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      _ <- use pool $ selectOneSession
      release pool
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

  it "Getting and setting session variables works" \connectionSettings ->
    withDefaultPool connectionSettings \pool -> do
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
      res <- use pool $ do
        setSettingSession "testing.foo" "hello world"
        getSettingSession "testing.foo"
      res `shouldBe` Right (Just "hello world")

  it "Session variables stay set when a connection gets reused" \connectionSettings ->
    withPool 1 10 1_800 1_800 connectionSettings \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")

  it "Releasing the pool resets session variables" \connectionSettings ->
    withPool 1 10 1_800 1_800 connectionSettings \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      release pool
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing

  it "Times out connection acquisition" \connectionSettings ->
    -- 1ms timeout
    withPool 1 0.001 1_800 1_800 connectionSettings \pool -> do
      sleeping <- newEmptyMVar
      t0 <- getCurrentTime
      res <-
        race
          ( use pool
              $ liftIO
              $ do
                putMVar sleeping ()
                -- 1s
                threadDelay 1_000_000
          )
          ( do
              takeMVar sleeping
              use pool $ selectOneSession
          )
      t1 <- getCurrentTime
      res `shouldBe` Right (Left AcquisitionTimeoutUsageError)
      -- 0.5s
      diffUTCTime t1 t0 `shouldSatisfy` (< 0.5)

  it "Passively times out old connections (maxLifetime)" \connectionSettings ->
    -- 0.5s connection lifetime
    withPool 1 10 0.5 1_800 connectionSettings \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
      threadDelay 1_000_000 -- 1s
      res3 <- use pool $ getSettingSession "testing.foo"
      res3 `shouldBe` Right Nothing

  it "Counts active connections" \_ -> do
    appName <- ("hasql-pool-test-" <>) . Text.pack . show <$> Random.uniformWord32 Random.globalStdGen
    Testcontainers.withConnectionSettingsAndAppName appName \(taggedConnectionSettings, _) -> do
      withPool 3 10 1_800 1_800 taggedConnectionSettings \pool -> do
        res <- use pool $ countConnectionsSession appName
        res `shouldBe` Right 1

  it "Actively times out old connections (maxLifetime)" \_ -> do
    appName <- ("hasql-pool-test-" <>) . Text.pack . show <$> Random.uniformWord32 Random.globalStdGen
    Testcontainers.withConnectionSettingsEx \(baseSettings, makeTagged) -> do
      let taggedConnectionSettings = makeTagged appName
      withDefaultPool baseSettings \countPool -> do
        withPool 3 10 0.5 1_800 taggedConnectionSettings \limitedPool -> do
          res <- use limitedPool $ selectOneSession
          res `shouldBe` Right 1
          res2 <- use countPool $ countConnectionsSession appName
          res2 `shouldBe` Right 1
          threadDelay 1_000_000 -- 1s
          res3 <- use countPool $ countConnectionsSession appName
          res3 `shouldBe` Right 0

  it "Times out old connections (maxIdletime)" \connectionSettings -> do
    -- 0.5s connection idle time
    withPool 1 10 1_800 0.5 connectionSettings \pool -> do
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

withPool :: Int -> DiffTime -> DiffTime -> DiffTime -> [Connection.Setting.Setting] -> (Pool -> IO a) -> IO a
withPool poolSize acqTimeout maxLifetime maxIdletime connectionSettings =
  bracket
    ( acquire
        ( Config.settings
            [ Config.size poolSize,
              Config.acquisitionTimeout acqTimeout,
              Config.agingTimeout maxLifetime,
              Config.idlenessTimeout maxIdletime,
              Config.staticConnectionSettings connectionSettings
            ]
        )
    )
    release

withDefaultPool :: [Connection.Setting.Setting] -> (Pool -> IO a) -> IO a
withDefaultPool = withPool 3 10 1_800 1_800
