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
--
-- == The standard a rule has to meet
--
-- A rule belongs here only if it can name the /residue/: state left behind
-- on this particular connection that a fresh connection would not have, and
-- that no subsequent session can clear. A rule that cannot name the residue
-- is a rule to delete.
--
-- The two errors are not symmetric. A wrong discard costs one reconnect,
-- paid once. A wrong retain hands back a connection whose client-side
-- caches - 'Hasql.ConnectionState.oidCache' and
-- 'Hasql.ConnectionState.statementCache' - disagree with the server, and
-- since nothing ever evicts from either, that connection fails the same
-- statement for the whole of its remaining lifetime. That asymmetry is the
-- reason a discard is the default cure below, not a reason to discard on
-- suspicion: the bar is still a nameable residue.
module Hasql.Pool.ConnectionDiscards where

import Data.Text qualified as Text
import Hasql.Errors qualified as Errors
import Hasql.Pool.Prelude

sessionError :: Errors.SessionError -> Bool
sessionError = \case
  -- Missing types leave no residue on the connection: the lookup is a live
  -- catalog query, nothing gets negatively cached, and the connection state
  -- comes back untouched. A fresh connection would ask the same catalog and
  -- get the same answer, so discarding buys nothing but a reconnect.
  Errors.MissingTypesSessionError {} -> False
  Errors.ScriptSessionError _ serverErr -> serverError serverErr
  Errors.StatementSessionError _ _ _ _ _ statementErr -> statementError statementErr

statementError :: Errors.StatementError -> Bool
statementError = \case
  Errors.ServerStatementError serverErr -> serverError serverErr
  -- The remaining constructors all describe a disagreement between the
  -- decoder the caller supplied and the result the server actually sent.
  -- The server raised no error, no cache was consulted or written, and the
  -- connection is in exactly the state it was in before the statement ran.
  -- A fresh connection re-runs the statement and fails identically, so a
  -- discard would cost a reconnect and change nothing.
  --
  -- 'UnexpectedColumnTypeStatementError' is worth singling out, because it
  -- used to discard. The reasoning was that a decoder built against an OID
  -- the catalog has since reassigned cannot succeed while the connection
  -- holds the old cache. That case is real, but it does not surface here:
  -- the server notices the dropped type first and raises @XX000@ (see
  -- 'serverError'), which arrives as a 'ServerStatementError'. What reaches
  -- this constructor instead is the ordinary mismatch - a decoder asking
  -- for @int4@ where the column is @text@ - which is a bug in the caller's
  -- code, not residue on the connection.
  --
  -- The match is deliberately exhaustive rather than a wildcard: when hasql
  -- adds a constructor, this module should fail to compile and force a
  -- verdict, instead of silently inheriting @False@.
  Errors.UnexpectedRowCountStatementError {} -> False
  Errors.UnexpectedColumnCountStatementError {} -> False
  Errors.UnexpectedColumnTypeStatementError {} -> False
  Errors.RowStatementError {} -> False
  Errors.UnexpectedResultStatementError {} -> False

-- | Verdict on a server error, keyed on its SQLSTATE.
--
-- The codes below are matched exactly unless a comment says otherwise.
-- Exactness is load-bearing in two places: @XX000@ discards while @XX001@
-- and @XX002@ do not, and @42P05@ discards while the rest of class @42@
-- does not.
serverError :: Errors.ServerError -> Bool
serverError (Errors.ServerError code _ _ _ _)
  -- @internal_error@. The target is @cache lookup failed for type NNNN@,
  -- raised when a prepared statement's plan references a type OID that a
  -- @drop type@ / @create type@ has since retired. Verified against a live
  -- server: every subsequent execution on that connection raises it again,
  -- forever. The residue is the server-side prepared statement bound to a
  -- dead OID, together with hasql's 'statementCache' entry that keeps
  -- pointing at it; both are session-scoped, so a fresh connection replans
  -- cleanly.
  --
  -- The code is broader than that: @XX000@ is also PostgreSQL's catch-all
  -- for internal faults that say nothing about the connection. Those get
  -- discarded too. That is a known over-reach, accepted because the code
  -- alone carries no way to tell the cases apart - the distinguishing
  -- signal is the message text, which the server localises under
  -- @lc_messages@ and which the pool therefore cannot rely on.
  | code == "XX000" = True
  -- @feature_not_supported@. The target is @cached plan must not change
  -- result type@, raised when a schema change alters the columns or
  -- parameter types a prepared statement was planned against. Verified
  -- against a live server: it too repeats on every subsequent execution.
  -- Same residue as @XX000@, same cure.
  --
  -- Same over-reach, too: plenty of statements asking for something
  -- PostgreSQL simply does not do raise @0A000@ on a perfectly healthy
  -- connection, and are discarded for nothing.
  | code == "0A000" = True
  -- @invalid_sql_statement_name@. Hasql's 'statementCache' is a
  -- client-side ledger of what it believes is prepared on this connection.
  -- This code means the ledger and the server disagree - the name hasql
  -- intends to execute is not there - and since nothing ever evicts from
  -- that ledger, the disagreement is permanent. Reproducible by
  -- deallocating a prepared statement and executing it afterwards.
  | code == "26000" = True
  -- @duplicate_prepared_statement@: the same ledger desynced the other way,
  -- with the server holding a name hasql thinks is free. Reachable when
  -- something outside hasql prepared a colliding name, or after a reset
  -- hasql did not observe. Same residue, same permanence.
  | code == "42P05" = True
  -- @in_failed_sql_transaction@. The connection is parked inside an aborted
  -- transaction, and every statement on it fails until a rollback. The pool
  -- does no I/O on the return path, so it cannot issue that rollback, which
  -- leaves a discard as the only cure available.
  | code == "25P02" = True
  -- Class @08@, @connection_exception@ - prefix match. Most of the class
  -- describes a connection that is already gone. @08P01@
  -- (@protocol_violation@) is the one that earns its place on residue
  -- grounds rather than liveness: client and server disagree about where
  -- they are in the wire protocol, and nothing on the connection can
  -- resynchronise them.
  --
  -- Expect most of this class to be unreachable in practice: when the
  -- socket dies hasql reports 'Errors.ConnectionUseError', which the pool
  -- discards without consulting this module at all. @08P01@ is the member
  -- worth keeping regardless, since a protocol desync need not come with a
  -- dead socket.
  | Text.isPrefixOf "08" code = True
  -- @statement_completion_unknown@. The one code in class @40@ that
  -- qualifies: the client does not know whether the statement committed, so
  -- the connection's transactional state is genuinely unknown rather than
  -- merely failed.
  | code == "40003" = True
  -- Class @57P@ - prefix match. @57P01@ (@admin_shutdown@), @57P02@
  -- (@crash_shutdown@), @57P03@ (@cannot_connect_now@), @57P04@
  -- (@database_dropped@), @57P05@ (@idle_session_timeout@), @57P06@
  -- (@idle_in_transaction_session_timeout@). All are FATAL: the server
  -- emits the error and closes the socket, so a connection that saw one is
  -- gone or going.
  --
  -- The prefix is @57P@ rather than @57@ on purpose. @57014@
  -- (@query_canceled@) must not match: a cancelled query leaves a perfectly
  -- healthy connection, and discarding on it would punish every caller-side
  -- timeout with a reconnect.
  --
  -- As with class @08@, some of these only ever arrive as
  -- 'Errors.ConnectionUseError', which the pool discards without consulting
  -- this module. @57P01@ was tested directly: terminating the backend
  -- underneath a session surfaces as a 'Errors.ConnectionUseError' carrying
  -- the FATAL text under @pqi-ffi@, and as a thrown exception under
  -- @pqi-native@ - never as a 'Errors.ServerError' reaching this function.
  -- The rule is kept anyway: it costs nothing, and the remaining codes in
  -- the class have not been shown to behave the same way.
  | Text.isPrefixOf "57P" code = True
  | otherwise = False

-- $rejected
--
-- Codes deliberately /not/ discarded, recorded here so they are not
-- reintroduced on the strength of sounding alarming. Each fails the
-- standard in the module header: the error is real, but the residue is not
-- on this connection.
--
-- * Class @53@, @insufficient_resources@ - @53000@, @53100@ (@disk_full@),
--   @53200@ (@out_of_memory@), @53400@. These describe a shortage on the
--   /server/. The connection that received the error is structurally
--   untouched, and a reconnect does not cure a full disk. Worse, under load
--   the condition fires on many connections at once, so a discard rule
--   turns a resource shortage into a reconnect storm against a server that
--   is already short of resources. @53300@ (@too_many_connections@) is
--   additionally unreachable here: it arrives at acquisition time as an
--   'Errors.AcquireError' and never reaches this module.
--
-- * @25001@, @active_sql_transaction@. Raised when a command that cannot
--   run inside a transaction block is issued inside one. A caller bug, on a
--   healthy connection, reproduced identically by a fresh one. Listed only
--   so nobody groups it with @25P02@ by prefix.
--
-- * @40001@ (@serialization_failure@) and @40P01@ (@deadlock_detected@).
--   The canonical retry-on-the-same-connection cases. Discarding these
--   would make every serialisation retry pay for a reconnect.
--
-- * @57014@, @query_canceled@. See the @57P@ rule above.
--
-- * @55P03@ (@lock_not_available@) and @55006@ (@object_in_use@). Pure
--   contention between sessions; nothing is left behind on either.
--
-- * @58030@ (@io_error@), @XX001@ (@data_corrupted@), @XX002@
--   (@index_corrupted@). Genuinely alarming, but the damage is on disk. A
--   fresh connection reads the same corrupt page.
--
-- * Class @54@ (@program_limit_exceeded@), class @22@ (data exceptions),
--   and class @42@ other than @42P05@ (syntax and access rule violations).
--   Statement bugs, not connection state.
