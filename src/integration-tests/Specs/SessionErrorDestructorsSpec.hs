module Specs.SessionErrorDestructorsSpec where

import Data.HashSet qualified as HashSet
import Hasql.Errors qualified as Errors
import Hasql.Pool.SessionErrorDestructors qualified as SessionErrorDestructors
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "requiresConnectionDiscard" do
    it "matches connection session errors" do
      SessionErrorDestructors.requiresConnectionDiscard (Errors.ConnectionSessionError "connection lost")
        `shouldBe` True

    it "matches cached plan server errors" do
      SessionErrorDestructors.requiresConnectionDiscard
        ( Errors.StatementSessionError
            1
            0
            "select 1"
            []
            True
            (Errors.ServerStatementError (Errors.ServerError "0A000" "cached plan must not change result type" Nothing Nothing Nothing))
        )
        `shouldBe` True

    it "matches cache lookup server errors in scripts" do
      SessionErrorDestructors.requiresConnectionDiscard
        (Errors.ScriptSessionError "select 1" (Errors.ServerError "XX000" "cache lookup failed for type 12345" Nothing Nothing Nothing))
        `shouldBe` True

    it "matches missing type errors" do
      SessionErrorDestructors.requiresConnectionDiscard
        (Errors.MissingTypesSessionError (HashSet.singleton (Nothing, "mood")))
        `shouldBe` True

    it "matches unexpected column type errors" do
      SessionErrorDestructors.requiresConnectionDiscard
        ( Errors.StatementSessionError
            1
            0
            "select value from test"
            []
            True
            (Errors.UnexpectedColumnTypeStatementError 0 42 43)
        )
        `shouldBe` True

    it "does not match ordinary server errors" do
      SessionErrorDestructors.requiresConnectionDiscard
        ( Errors.StatementSessionError
            1
            0
            "select 1"
            []
            True
            (Errors.ServerStatementError (Errors.ServerError "23505" "duplicate key value violates unique constraint" Nothing Nothing Nothing))
        )
        `shouldBe` False
