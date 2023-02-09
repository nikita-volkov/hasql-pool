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
  connectionSettings <- getConnectionSettings
  let config = setCapacity 3 . setConnectionSettings connectionSettings $ defaultConfig
      withPool modifyConf = bracket
        (acquireConf (modifyConf config))
        release

  hspec . describe "" $ do
    it "Releases a spot in the pool when there is a query error" $ withPool id $ \pool -> do
      use pool badQuerySession `shouldNotReturn` (Right ())
      use pool selectOneSession `shouldReturn` (Right 1)
    it "Simulation of connection error works" $ withPool id $ \pool -> do
      res <- use pool $ closeConnSession >> selectOneSession
      shouldSatisfy res $ \case
        Left (SessionUsageError (Session.QueryError _ _ (Session.ClientError _))) -> True
        _ -> False
    it "Connection errors cause eviction of connection" $ withPool id $ \pool -> do
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "Connection gets returned to the pool after normal use" $ withPool id $ \pool -> do
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "Connection gets returned to the pool after non-connection error" $ withPool id $ \pool -> do
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "The pool remains usable after release" $ withPool id $ \pool -> do
      res <- use pool $ selectOneSession
      release pool
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "Getting and setting session variables works" $ withPool id $ \pool -> do
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
      res <- use pool $ do
        setSettingSession "testing.foo" "hello world"
        getSettingSession "testing.foo"
      res `shouldBe` Right (Just "hello world")
    it "Session variables stay set when a connection gets reused" $ withPool (setCapacity 1) $ \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
    it "Releasing the pool resets session variables" $ withPool (setCapacity 1) $ \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      release pool
      res <- use pool $ getSettingSession "testing.foo"
      res `shouldBe` Right Nothing
    it "Times out connection acquisition" $
        -- 1ms timeout
        withPool (setCapacity 1 . setAcquisitionTimeout (Just 1000)) $ \pool -> do
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
    it "Passively times out old connections" $
        -- 0.5s connection lifetime
        withPool (setCapacity 1 . setMaxLifetime (Just 500000)) $ \pool -> do
      res <- use pool $ setSettingSession "testing.foo" "hello world"
      res `shouldBe` Right ()
      res2 <- use pool $ getSettingSession "testing.foo"
      res2 `shouldBe` Right (Just "hello world")
      threadDelay 1000000 -- 1s
      res3 <- use pool $ getSettingSession "testing.foo"
      res3 `shouldBe` Right Nothing
    it "Counts active connections" $ do
      (taggedConnectionSettings, appName) <- tagConnection connectionSettings
      pool <- acquireConf (setConnectionSettings taggedConnectionSettings config)
      res <- use pool $ countConnectionsSession appName
      res `shouldBe` Right 1
      release pool
    it "Times out old connections" $ do
      (taggedConnectionSettings, appName) <- tagConnection connectionSettings
      limitedPool <- acquireConf (setMaxLifetime (Just 500000) . setConnectionSettings taggedConnectionSettings $ config)
      countPool <- acquireConf config
      res <- use limitedPool $ selectOneSession
      res `shouldBe` Right 1
      res2 <- use countPool $ countConnectionsSession appName
      res2 `shouldBe` Right 1
      threadDelay 1000000 -- 1s
      res3 <- use countPool $ countConnectionsSession appName
      res3 `shouldBe` Right 0


getConnectionSettings :: IO Connection.Settings
getConnectionSettings = B8.unwords . catMaybes
    <$> sequence
      [ setting "host" $ defaultEnv "POSTGRES_HOST" "localhost",
        setting "port" $ defaultEnv "POSTGRES_PORT" "5432",
        setting "user" $ defaultEnv "POSTGRES_USER" "postgres",
        setting "password" $ maybeEnv "POSTGRES_PASSWORD",
        setting "dbname" $ defaultEnv "POSTGRES_DBNAME" "postgres"
      ]
  where
    maybeEnv env = fmap B8.pack <$> System.Environment.lookupEnv env
    defaultEnv env val = Just . fromMaybe val <$> maybeEnv env
    setting label getEnv = do
      val <- getEnv
      return $ (\v -> label <> "=" <> v) <$> val

tagConnection :: Connection.Settings -> IO (Connection.Settings, Text)
tagConnection connectionSettings = do
  tag <- Random.uniformWord32 Random.globalStdGen
  let appName = "hasql-pool-test-" <> show tag
  return (connectionSettings <> " application_name=" <> B8.pack appName, Text.pack appName)

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
