module Main where

import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import Test.Hspec
import Prelude
import qualified System.Environment
import qualified Data.ByteString.Char8 as B8

main = do
  connectionSettings <- getConnectionSettings
  putStrLn $ show connectionSettings
  hspec $ describe "" $ do
    it "Releases a spot in the pool when there is a query error" $ do
      pool <- acquire 1 connectionSettings
      use pool badQuerySession `shouldNotReturn` (Right ())
      use pool selectOneSession `shouldReturn` (Right 1)
    it "Simulation of connection error works" $ do
      pool <- acquire 3 connectionSettings
      res <- use pool $ closeConnSession >> selectOneSession
      shouldSatisfy res $ \case
        Left (SessionUsageError (Session.QueryError _ _ (Session.ClientError _))) -> True
        _ -> False
    it "Connection errors cause eviction of connection" $ do
      pool <- acquire 3 connectionSettings
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ closeConnSession >> selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "Connection gets returned to the pool after normal use" $ do
      pool <- acquire 3 connectionSettings
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight
    it "Connection gets returned to the pool after non-connection error" $ do
      pool <- acquire 3 connectionSettings
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ badQuerySession
      res <- use pool $ selectOneSession
      shouldSatisfy res $ isRight

getConnectionSettings :: IO Connection.Settings
getConnectionSettings = B8.unwords . catMaybes <$> sequence
  [ setting "host" $ defaultEnv "POSTGRES_HOST" "localhost"
  , setting "port" $ defaultEnv "POSTGRES_PORT" "5432"
  , setting "user" $ defaultEnv "POSTGRES_USER" "postgres"
  , setting "password" $ maybeEnv "POSTGRES_PASSWORD"
  , setting "dbname" $ defaultEnv "POSTGRES_DBNAME" "postgres"
  ]
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
    statement = Statement.Statement "" Encoders.noParams Decoders.noResult True

closeConnSession :: Session.Session ()
closeConnSession = do
  conn <- ask
  liftIO $ Connection.release conn
