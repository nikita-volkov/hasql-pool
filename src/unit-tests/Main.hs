module Main (main) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Errors qualified as Errors
import Hasql.Pool.ConnectionDiscards qualified as ConnectionDiscards
import Prelude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serverError" do
    describe "Discards" do
      traverse_
        discards
        [ ("XX000", "internal_error: stale plan against a retired type OID"),
          ("0A000", "feature_not_supported: cached plan must not change result type"),
          ("26000", "invalid_sql_statement_name: statement cache ledger desync"),
          ("42P05", "duplicate_prepared_statement: the same desync inverted"),
          ("25P02", "in_failed_sql_transaction: parked in an aborted transaction"),
          ("40003", "statement_completion_unknown: commit status unknown"),
          ("08000", "connection_exception"),
          ("08001", "sqlclient_unable_to_establish_sqlconnection"),
          ("08003", "connection_does_not_exist"),
          ("08004", "sqlserver_rejected_establishment_of_sqlconnection"),
          ("08006", "connection_failure"),
          ("08007", "transaction_resolution_unknown"),
          ("08P01", "protocol_violation: client and server disagree on wire state"),
          ("57P01", "admin_shutdown"),
          ("57P02", "crash_shutdown"),
          ("57P03", "cannot_connect_now"),
          ("57P04", "database_dropped"),
          ("57P05", "idle_session_timeout"),
          ("57P06", "idle_in_transaction_session_timeout")
        ]

    -- The rejections matter more than the acceptances: they are the rules
    -- most likely to be reintroduced on the strength of sounding alarming.
    describe "Retains" do
      traverse_
        retains
        [ ("53000", "insufficient_resources: a server-side shortage, not residue"),
          ("53100", "disk_full: a reconnect does not empty the disk"),
          ("53200", "out_of_memory: discarding turns a crunch into a reconnect storm"),
          ("53300", "too_many_connections: an acquisition-time error to begin with"),
          ("53400", "configuration_limit_exceeded"),
          ("25001", "active_sql_transaction: a caller bug on a healthy connection"),
          ("40001", "serialization_failure: the canonical same-connection retry"),
          ("40P01", "deadlock_detected: likewise"),
          ("57014", "query_canceled: must not be swept up by the 57P rule"),
          ("55P03", "lock_not_available: contention, not residue"),
          ("55006", "object_in_use: likewise"),
          ("58030", "io_error: the damage is on disk"),
          ("XX001", "data_corrupted: must not be swept up by the XX000 rule"),
          ("XX002", "index_corrupted: likewise"),
          ("54000", "program_limit_exceeded"),
          ("22012", "division_by_zero"),
          ("23505", "unique_violation"),
          ("42601", "syntax_error: must not be swept up by the 42P05 rule"),
          ("42P01", "undefined_table: likewise"),
          ("0A001", "a neighbour of 0A000, which is matched exactly")
        ]

  describe "statementError" do
    it "Retains on an unexpected column type, which is a decoder bug rather than residue" do
      ConnectionDiscards.statementError (Errors.UnexpectedColumnTypeStatementError 0 23 25)
        `shouldBe` False
    it "Retains on an unexpected row count" do
      ConnectionDiscards.statementError (Errors.UnexpectedRowCountStatementError 1 1 2)
        `shouldBe` False
    it "Retains on an unexpected column count" do
      ConnectionDiscards.statementError (Errors.UnexpectedColumnCountStatementError 2 3)
        `shouldBe` False
    it "Retains on an unexpected result" do
      ConnectionDiscards.statementError (Errors.UnexpectedResultStatementError "whatever")
        `shouldBe` False
    it "Defers to the server error it wraps" do
      ConnectionDiscards.statementError (Errors.ServerStatementError (serverErrorWithCode "XX000"))
        `shouldBe` True

  describe "sessionError" do
    it "Retains on missing types, whose lookup leaves the connection untouched" do
      ConnectionDiscards.sessionError (Errors.MissingTypesSessionError mempty)
        `shouldBe` False
    it "Defers to the server error a failed script carries" do
      ConnectionDiscards.sessionError (Errors.ScriptSessionError "select 1" (serverErrorWithCode "26000"))
        `shouldBe` True
    it "Defers to the statement error a failed statement carries" do
      ConnectionDiscards.sessionError
        (Errors.StatementSessionError 1 0 "select 1" [] False (Errors.UnexpectedRowCountStatementError 1 1 0))
        `shouldBe` False

discards :: (Text, String) -> Spec
discards (code, why) =
  it ("Discards on " <> Text.unpack code <> " - " <> why) do
    ConnectionDiscards.serverError (serverErrorWithCode code) `shouldBe` True

retains :: (Text, String) -> Spec
retains (code, why) =
  it ("Retains on " <> Text.unpack code <> " - " <> why) do
    ConnectionDiscards.serverError (serverErrorWithCode code) `shouldBe` False

serverErrorWithCode :: Text -> Errors.ServerError
serverErrorWithCode code =
  Errors.ServerError code "synthetic" Nothing Nothing Nothing
