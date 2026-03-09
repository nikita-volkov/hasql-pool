module Hasql.Pool.SessionErrorDestructors where

import Hasql.Errors qualified as Errors
import Hasql.Pool.Prelude

reset :: (Text -> x) -> x -> Errors.SessionError -> x
reset onReset onNoReset = \case
  Errors.ConnectionSessionError details -> onReset details
  _ -> onNoReset

requiresConnectionDiscard :: Errors.SessionError -> Bool
requiresConnectionDiscard = \case
  Errors.ConnectionSessionError {} -> True
  Errors.MissingTypesSessionError {} -> True
  Errors.ScriptSessionError _ serverError -> isStaleServerError serverError
  Errors.StatementSessionError _ _ _ _ _ statementError -> statementRequiresConnectionDiscard statementError
  Errors.DriverSessionError {} -> False

discardDetails :: Errors.SessionError -> Maybe Text
discardDetails err =
  if requiresConnectionDiscard err
    then Just $ Errors.toMessage err
    else Nothing

statementRequiresConnectionDiscard :: Errors.StatementError -> Bool
statementRequiresConnectionDiscard = \case
  Errors.ServerStatementError serverError -> isStaleServerError serverError
  Errors.UnexpectedColumnTypeStatementError {} -> True
  _ -> False

isStaleServerError :: Errors.ServerError -> Bool
isStaleServerError (Errors.ServerError code _ _ _ _) =
  code == "0A000" || code == "XX000"
