module Main where

import Control.Concurrent.Async (race)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified System.Environment
import qualified System.Random as Random
import qualified System.Random.Stateful as Random
import Test.Hspec
import Prelude

main = do
  (connectionSettings, appName) <- getConnectionSettings
  let config = setCapacity 3 . setConnectionSettings connectionSettings $ defaultConfig
  hspec . describe "" $ do
    it "Releases a spot in the pool when there is a query error" $ do
      pool <- acquireConf (setCapacity 1 config)
      use pool badQuerySession `shouldNotReturn` (Right ())
      use pool selectOneSession `shouldReturn` (Right 1)
      release pool
    it "Simulation of connection error works" $ do
      pool <- acquireConf config
      res <- use pool $ closeConnSession >> selectOneSession
      shouldSatisfy res $ \case
        Left (SessionUsageError (Session.QueryError _ _ (Session.ClientError _))) -> True
        _ -> False
      release pool
    it "Connection errors cause eviction of connection" $ do
      pool <- acquireConf config
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
      release pool
    it "Connection gets returned to the pool after normal use" $ do
      pool <- acquireConf config
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
      release pool
    it "Connection gets returned to the pool after non-connection error" $ do
      pool <- acquireConf config
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
      release pool
    it "The pool remains usable after release" $ do
      pool <- acquireConf config
      res <- use pool $ selectOneSession
      release pool
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
      release pool
    it "Getting and setting session variables works" $ do
      pool <- acquireConf (setCapacity 1 config)
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
      res <- use pool $ do
        setSettingSession "testing.foo" "hello world"
        getSettingSession "testing.foo"
      res `shouldBe` Right (Just "hello world")
      release pool
    it "Session variables stay set when a connection gets reused" $ do
      pool <- acquireConf (setCapacity 1 config)
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
      release pool
    it "Releasing the pool resets session variables" $ do
      pool <- acquireConf (setCapacity 1 config)
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      release pool
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
      release pool
    it "Times out connection acquisition" $ do
      pool <- acquireConf (setCapacity 1 . setAcquisitionTimeout (Just 1000) $ config) -- 1ms timeout
      sleeping <- newEmptyMVar
      t0 <- getCurrentTime
      res <-
        race
          ( use pool $
              liftIO $ do
                putMVar sleeping ()
                threadDelay 1000000 -- 1s
          )
          ( do
              takeMVar sleeping
              use pool $ selectOneSession
          )
      t1 <- getCurrentTime
      res `shouldBe` Right (Left AcquisitionTimeoutUsageError)
      diffUTCTime t1 t0 `shouldSatisfy` (< 0.5) -- 0.5s
      release pool
    it "Times out old connections" $ do
      pool <- acquireConf (setCapacity 1 . setMaxLifetime (Just 500000) $ config) -- 0.5s connection lifetime
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
      threadDelay 1000000 -- 1s
      res3 <- use pool $ getSettingSession "testing.foo"
      res3 `shouldBe` Right Nothing
      release pool
    it "Counts active connections" $ do
      pool <- acquireConf (setCapacity 1 config)
      res <- use pool $ countConnectionsSession appName
      res `shouldBe` Right 1
      release pool


getConnectionSettings :: IO (Connection.Settings, Text)
getConnectionSettings = do
  connStr <- B8.unwords . catMaybes
    <$> sequence
      [ setting "host" $ defaultEnv "POSTGRES_HOST" "localhost",
        setting "port" $ defaultEnv "POSTGRES_PORT" "5432",
        setting "user" $ defaultEnv "POSTGRES_USER" "postgres",
        setting "password" $ maybeEnv "POSTGRES_PASSWORD",
        setting "dbname" $ defaultEnv "POSTGRES_DBNAME" "postgres"
      ]
  tag <- Random.uniformWord32 Random.globalStdGen
  let appName = "hasql-pool-test-" <> show tag
  return (connStr <> " application_name=" <> B8.pack appName, Text.pack appName)
  where
    maybeEnv env = fmap B8.pack <$> System.Environment.lookupEnv env
    defaultEnv env val = Just . fromMaybe val <$> maybeEnv env
    setting label getEnv = do
      val <- getEnv
      return $ (\v -> label <> "=" <> v) <$> val

selectOneSession :: Session.Session Int64
selectOneSession =
  Session.statement () statement
  where
    statement = Statement.Statement "SELECT 1" Encoders.noParams decoder True
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

badQuerySession :: Session.Session ()
badQuerySession =
  Session.statement () statement
  where
    statement = Statement.Statement "zzz" Encoders.noParams Decoders.noResult True

closeConnSession :: Session.Session ()
closeConnSession = do
  conn <- ask
  liftIO $ Connection.release conn

setSettingSession :: Text -> Text -> Session.Session ()
setSettingSession name value = do
  Session.statement (name, value) statement
  where
    statement = Statement.Statement "SELECT set_config($1, $2, false)" encoder Decoders.noResult True
    encoder =
      contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
        <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text))

getSettingSession :: Text -> Session.Session (Maybe Text)
getSettingSession name = do
  Session.statement name statement
  where
    statement = Statement.Statement "SELECT current_setting($1, true)" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text))

countConnectionsSession :: Text -> Session.Session Int64
countConnectionsSession appName = do
  Session.statement appName statement
  where
    statement = Statement.Statement "SELECT count(*) FROM pg_stat_activity WHERE application_name = $1" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
