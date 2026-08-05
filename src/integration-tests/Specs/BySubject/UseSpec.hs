module Specs.BySubject.UseSpec where

import Control.Concurrent.Async (race)
import Data.Text qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Pool
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Releases a spot in the pool when there is a query error" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool Sessions.badQuery `shouldNotReturn` (Right ())
      use pool Sessions.selectOne `shouldReturn` (Right 1)

  it "Connection errors cause eviction of connection" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      _ <- use pool $ Sessions.closeConn >> Sessions.selectOne
      _ <- use pool $ Sessions.closeConn >> Sessions.selectOne
      _ <- use pool $ Sessions.closeConn >> Sessions.selectOne
      res <- use pool $ Sessions.selectOne
      shouldSatisfy res $ isRight

  it "Driver errors cause eviction of connection" \scopeParams -> do
    settingName <- Scripts.generateVarname
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool (Sessions.setSetting settingName "present") `shouldReturn` Right ()
      result <- use pool driverError
      result `shouldSatisfy` \case
        Left (SessionUsageError (Errors.DriverSessionError _)) -> True
        _ -> False
      use pool (Sessions.getSetting settingName) `shouldReturn` Right Nothing

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

  -- https://github.com/nikita-volkov/hasql-pool/issues/38
  --
  -- When a session is interrupted by an asynchronous exception (e.g., a
  -- caller-side timeout racing the query, as simulated here via `race`)
  -- while it is genuinely blocked waiting on the server's response, the
  -- underlying libpq connection is left mid-command: the query was sent,
  -- but its result was never read. `onLiveConn` in Hasql.Pool.use still
  -- unconditionally returns such a connection to the pool (the `Left exc`
  -- branch calls `returnConn` for all exceptions, not just synchronous
  -- ones), so the next `use` call hands out a connection whose protocol
  -- state is desynced from libpq's expectations. This is a plausible root
  -- cause of the "connection pointer is NULL" reports: two independent
  -- consumers of hasql-pool end up driving the same libpq connection state
  -- machine without coordination.
  it "Does not return a connection to the pool when the session is interrupted by an asynchronous exception" \scopeParams ->
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      started <- newEmptyMVar
      _ <-
        race
          ( use pool do
              liftIO $ putMVar started ()
              Sessions.sleep 2
          )
          ( do
              takeMVar started
              -- Give the query time to actually reach the server and for
              -- the client to start blocking on the socket read, as
              -- opposed to being cancelled while still sending.
              threadDelay 200_000
          )
      res <- use pool Sessions.selectOne
      res `shouldSatisfy` isRight

  it "Cached type errors cause eviction of connection" \scopeParams -> do
    typeName <- Text.replace "-" "_" <$> Scripts.generateName "cached_type_"
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool (Session.script (createTypeSql typeName)) `shouldReturn` Right ()
      use pool (roundtripEnum typeName "ok") `shouldReturn` Right "ok"
      use pool (Session.script (recreateTypeSql typeName)) `shouldReturn` Right ()
      res <- use pool (roundtripEnum typeName "ok")
      shouldSatisfy res \case
        Left (SessionUsageError _) -> True
        _ -> False
      use pool (roundtripEnum typeName "ok") `shouldReturn` Right "ok"

quoteIdentifier :: Text -> Text
quoteIdentifier identifier =
  "\"" <> Text.replace "\"" "\"\"" identifier <> "\""

createTypeSql :: Text -> Text
createTypeSql typeName =
  "create type " <> quotedTypeName <> " as enum ('sad', 'ok', 'happy')"
  where
    quotedTypeName = quoteIdentifier typeName

recreateTypeSql :: Text -> Text
recreateTypeSql typeName =
  "drop type " <> quotedTypeName <> "; create type " <> quotedTypeName <> " as enum ('sad', 'ok', 'happy')"
  where
    quotedTypeName = quoteIdentifier typeName

roundtripEnum :: Text -> Text -> Session.Session Text
roundtripEnum typeName value =
  Session.statement value statement
  where
    statement =
      Statement.preparable
        ("select $1 :: " <> quoteIdentifier typeName)
        (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing typeName id)))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing typeName Just))))

driverError :: Session.Session ()
driverError =
  Session.onLibpqConnection \connection ->
    pure (Left (Errors.DriverSessionError "synthetic driver error"), connection)
