module Specs.BySubject.UseSpec where

import Data.Text qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Test.Hspec
import Prelude

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

  it "Cached type errors cause eviction of connection" \scopeParams -> do
    typeName <- Scripts.generateName "cached-type-"
    Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams \_ pool -> do
      use pool (Session.script (createTypeSql typeName)) `shouldReturn` Right ()
      use pool (roundtripEnum typeName "ok") `shouldReturn` Right "ok"
      use pool (Session.script (recreateTypeSql typeName)) `shouldReturn` Right ()
      use pool (roundtripEnum typeName "ok")
        `shouldSatisfy` \case
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
