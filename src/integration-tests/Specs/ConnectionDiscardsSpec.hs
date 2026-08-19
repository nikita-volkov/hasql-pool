-- | Reachability and recovery coverage for
-- 'Hasql.Pool.ConnectionDiscards'.
--
-- The verdicts themselves are pure functions of a SQLSTATE and are pinned by
-- the @unit-tests@ suite, which needs no server. What only a live server can
-- establish is the two things asserted here:
--
-- * that the code in question actually reaches the predicate as a
--   'Errors.SessionUseError', rather than arriving as a
--   'Errors.ConnectionUseError' the pool discards without consulting the
--   module at all, and
--
-- * that the discard restores service - the residue really was
--   connection-local, and a fresh connection runs the same statement
--   successfully.
--
-- Every test therefore runs against a pool of size 1, so "the next use" is
-- guaranteed to be either the same connection or its replacement.
module Specs.ConnectionDiscardsSpec where

import Data.Text qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Helpers.Sessions qualified as Sessions
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  -- XX000. The server notices the retired type OID while replanning the
  -- prepared statement and raises @cache lookup failed for type NNNN@, which
  -- repeats on every subsequent execution over that connection.
  it "Discards a connection whose prepared statement references a recreated type" \scopeParams -> do
    typeName <- Text.replace "-" "_" <$> Scripts.generateName "cached_type_"
    onSingleConnectionPool scopeParams \pool -> do
      use pool (Session.script (createTypeSql typeName)) `shouldReturn` Right ()
      use pool (roundtripEnum typeName "ok") `shouldReturn` Right "ok"
      use pool (Session.script (recreateTypeSql typeName)) `shouldReturn` Right ()
      shouldFailWithSessionError (use pool (roundtripEnum typeName "ok"))
      use pool (roundtripEnum typeName "ok") `shouldReturn` Right "ok"

  -- 0A000, @cached plan must not change result type@. The prepared plan for
  -- @select *@ was built against one column; adding a second one invalidates
  -- it permanently for this connection.
  it "Discards a connection whose prepared plan outlived the shape of its result" \scopeParams -> do
    tableName <- Text.replace "-" "_" <$> Scripts.generateName "cached_plan_"
    onSingleConnectionPool scopeParams \pool -> do
      use pool (Session.script ("create table " <> quoteIdentifier tableName <> " (a int8)"))
        `shouldReturn` Right ()
      use pool (selectStar tableName) `shouldReturn` Right ()
      use pool (Session.script ("alter table " <> quoteIdentifier tableName <> " add column b int8"))
        `shouldReturn` Right ()
      shouldFailWithSessionError (use pool (selectStar tableName))
      -- The replacement connection replans against the new shape, so the
      -- one-column decoder now genuinely mismatches. That it fails
      -- \*differently* is the point: the plan-cache residue is gone.
      use pool (selectTwoColumns tableName) `shouldReturn` Right ()

  -- 26000, @prepared statement "..." does not exist@. Hasql's statement
  -- cache still lists the name; the server no longer does, and nothing
  -- evicts the stale entry.
  it "Discards a connection whose statement cache outlived the server's prepared statements" \scopeParams ->
    onSingleConnectionPool scopeParams \pool -> do
      use pool Sessions.selectOne `shouldReturn` Right 1
      use pool (Session.script "deallocate all") `shouldReturn` Right ()
      shouldFailWithSessionError (use pool Sessions.selectOne)
      use pool Sessions.selectOne `shouldReturn` Right 1

  -- 25P02. Note the sequencing: the statement that aborts the transaction
  -- fails with a syntax error, which is correctly *retained* - the residue
  -- only becomes visible on the following use, which is the one that
  -- triggers the discard.
  it "Discards a connection left parked in an aborted transaction" \scopeParams ->
    onSingleConnectionPool scopeParams \pool -> do
      shouldFailWithSessionError (use pool (Session.script "begin" >> Sessions.badQuery))
      shouldFailWithSessionError (use pool Sessions.selectOne)
      use pool Sessions.selectOne `shouldReturn` Right 1

  -- 57P01. Measured, not assumed: terminating the backend surfaces as a
  -- 'Errors.ConnectionUseError' carrying the FATAL text under @pqi-ffi@ and
  -- as a thrown exception under @pqi-native@ - in neither case as a server
  -- error whose SQLSTATE reaches 'ConnectionDiscards.serverError'. The
  -- @57P@ rule is therefore dead for this code, and is kept only because
  -- the rest of the class has not been shown to behave the same way. What
  -- this test asserts is the outcome all those paths share.
  it "Recovers from a backend terminated underneath it" \scopeParams ->
    onSingleConnectionPool scopeParams \pool -> do
      use pool Sessions.selectOne `shouldReturn` Right 1
      _ <- try @SomeException (use pool terminateSelf)
      use pool Sessions.selectOne `shouldReturn` Right 1

onSingleConnectionPool :: Scripts.ScopeParams -> (Pool -> IO ()) -> IO ()
onSingleConnectionPool scopeParams cont =
  Scripts.onAutotaggedPool 1 10 1_800 1_800 scopeParams (const cont)

shouldFailWithSessionError :: (Show a) => IO (Either UsageError a) -> Expectation
shouldFailWithSessionError action = do
  res <- action
  shouldSatisfy res \case
    Left (SessionUsageError _) -> True
    _ -> False

quoteIdentifier :: Text -> Text
quoteIdentifier identifier =
  "\"" <> Text.replace "\"" "\"\"" identifier <> "\""

createTypeSql :: Text -> Text
createTypeSql typeName =
  "create type " <> quoteIdentifier typeName <> " as enum ('sad', 'ok', 'happy')"

recreateTypeSql :: Text -> Text
recreateTypeSql typeName =
  "drop type "
    <> quoteIdentifier typeName
    <> "; create type "
    <> quoteIdentifier typeName
    <> " as enum ('sad', 'ok', 'happy')"

roundtripEnum :: Text -> Text -> Session.Session Text
roundtripEnum typeName value =
  Session.statement value statement
  where
    statement =
      Statement.preparable
        ("select $1 :: " <> quoteIdentifier typeName)
        (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing typeName id)))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing typeName Just))))

selectStar :: Text -> Session.Session ()
selectStar tableName =
  Session.statement () statement
  where
    statement =
      Statement.preparable
        ("select * from " <> quoteIdentifier tableName)
        Encoders.noParams
        (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8)) $> ())

selectTwoColumns :: Text -> Session.Session ()
selectTwoColumns tableName =
  Session.statement () statement
  where
    statement =
      Statement.preparable
        ("select * from " <> quoteIdentifier tableName)
        Encoders.noParams
        ( Decoders.rowList
            ( (,)
                <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            )
            $> ()
        )

terminateSelf :: Session.Session ()
terminateSelf =
  Session.script "select pg_terminate_backend(pg_backend_pid())"
