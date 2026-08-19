-- | Classification of 'Errors.SessionError' along the axis the pool cares
-- about: whether the connection it came back on is still fit to reuse.
--
-- Hasql's own verdict - 'Errors.SessionUseError' - only promises the
-- connection is still usable in the sense that 'Hasql.Connection.use' will
-- run another session on it without throwing. It does not promise the
-- statement that just failed can ever succeed on the same connection: a
-- stale catalog cache or a plan built against a type that no longer exists
-- leaves the connection alive but unable to replan that statement. Hasql
-- declined to own this call - see the removal of
-- 'Hasql.Errors.IsError.isTransient' - so the pool keeps making it itself,
-- the same way it always did.
--
-- 'Errors.ConnectionUseError' needs no classification here: it always means
-- the connection is gone, and hasql has already finished it.
module Hasql.Pool.SessionErrorDestructors where

import Hasql.Errors qualified as Errors
import Hasql.Pool.Prelude

sessionRequiresConnectionDiscard :: Errors.SessionError -> Bool
sessionRequiresConnectionDiscard = \case
  -- Missing types leave no residue on the connection: the lookup is a live
  -- catalog query, nothing gets negatively cached, and the connection state
  -- comes back untouched. A fresh connection would ask the same catalog and
  -- get the same answer, so discarding buys nothing but a reconnect.
  Errors.MissingTypesSessionError {} -> False
  Errors.ScriptSessionError _ serverError -> isStaleServerError serverError
  Errors.StatementSessionError _ _ _ _ _ statementError -> statementRequiresConnectionDiscard statementError

statementRequiresConnectionDiscard :: Errors.StatementError -> Bool
statementRequiresConnectionDiscard = \case
  Errors.ServerStatementError serverError -> isStaleServerError serverError
  Errors.UnexpectedColumnTypeStatementError {} -> True
  _ -> False

isStaleServerError :: Errors.ServerError -> Bool
isStaleServerError (Errors.ServerError code _ _ _ _) =
  code == "0A000" || code == "XX000"
