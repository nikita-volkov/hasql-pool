# v1.6.0.0

## Breaking

- Migrate to `hasql-2.1`. `UsageError`'s `ConnectionUsageError` now wraps `Errors.AcquireError` (was `Errors.ConnectionError`), and `SessionUsageError` now wraps `Errors.UseError` (was `Errors.SessionError`), matching hasql's merge of its connection-fatal error constructors into a single `UseError` type. Code pattern-matching into the wrapped error needs an extra step through `SessionUseError`/`ConnectionUseError` to reach what used to be there directly.

## Fixes

- A session that fails with `MissingTypesSessionError` no longer causes the connection to be discarded. The type lookup behind that error is a live catalog query which leaves no residue on the connection - nothing is negatively cached and the connection state comes back untouched - so a fresh connection would ask the same catalog and get the same answer. Referencing a type that does not exist used to cost a full teardown and reconnect per attempt.

- A session that fails with `UnexpectedColumnTypeStatementError` no longer causes the connection to be discarded. That rule was there for decoders built against an OID the catalog had since reassigned, but that case never reaches this error: the server notices the retired type first and raises `XX000`. What does reach it is the ordinary decoder mismatch - asking for `int4` where the column is `text` - which is a caller-side bug that a fresh connection reproduces identically.

- Connections are now discarded on `26000` (`invalid_sql_statement_name`) and `42P05` (`duplicate_prepared_statement`). Both mean hasql's per-connection statement cache and the server disagree about what is prepared, and since nothing evicts from that cache, the disagreement is permanent - every subsequent execution of the affected statement fails for the rest of the connection's life.

- Connections are now discarded on `25P02` (`in_failed_sql_transaction`), which means the connection is parked inside an aborted transaction where every statement fails until a rollback the pool has no opportunity to issue.

- Connections are now discarded on class `08` (`connection_exception`, including `08P01` `protocol_violation`), on `40003` (`statement_completion_unknown`), and on class `57P` (`admin_shutdown`, `crash_shutdown`, `database_dropped`, the idle-session timeouts, and the rest). Note that `57014` (`query_canceled`) is deliberately excluded - a cancelled query leaves a perfectly healthy connection.

  Codes deliberately left retaining, with the reasoning recorded in `Hasql.Pool.ConnectionDiscards`, include class `53` (`insufficient_resources` - a server-side shortage that a reconnect cannot cure, and where discarding would turn a resource crunch into a reconnect storm), `40001`/`40P01` (the canonical same-connection retry cases), `55P03`/`55006`, `58030`, and `XX001`/`XX002`.

- A session interrupted by an asynchronous exception (a caller-side `timeout`, `race`, or `killThread`) is now discarded instead of returned to the pool. Hasql finishes the connection on that path itself, so re-queuing it as before handed the next caller a spent handle - a plausible root cause of "connection pointer is NULL" reports (#38).

  As a consequence, `initSession` no longer survives such an exception - it now only survives ordinary session errors (a bad query, a constraint violation), since those are the only case that leaves the old connection in play.
# v1.5.1.1

## Fixes

- After the server closes an idle pooled connection (a Postgres restart,
  `pg_terminate_backend`, a server-side idle timeout), the first session on
  it can fail with `UnexpectedRowCountStatementError` instead of a connection
  error. Hasql maps the extra result that reports the closed connection to
  that error, and a `singleRow` statement that returns two rows fails the
  same way on a healthy connection, so the error alone cannot justify a
  discard: the dead connection got returned to the pool and failed one more
  session before it was discarded, so after a database restart every pooled
  connection failed two sessions. Now, when a session fails with an error
  that does not require a discard, `use` also checks the status that the
  adapter reports for the connection and discards it when it is marked bad.

# v1.5.1.0

## Fixes

- Fix pool capacity leak when `use` gets interrupted during connection
  establishment, connection refresh, or an observation (#60)
- Fix pool state corruption from throwing observation handlers; handlers
  are now contained (see the updated `observationHandler` docs)
- Fix background reaper dying permanently from a failed connection
  release or observation
- Fix `release` leaking flushed entries on failure
- Fix waiters waiting forever on a garbage-collected pool

## Added

- `reuseOrder` setting (#59): LIFO reuse lets the pool shrink to the
  connections the load needs and keeps caches warm; FIFO stays the default
- One background timer for acquisition timeouts instead of one
  uncancelable timer per waiting request (#58)

## Changed

- Connections are handed to waiting requests in arrival order; a request
  no longer takes a connection while others wait (#58)
# v1.5.0.1

- Allow newer pqi.

# v1.5.0.0

## Breaking

- Migrate to `hasql-2`, replacing `postgresql-libpq` with the `pqi` connection-adapter interface. `acquire` now takes a `Pqi.Adapter` as its first argument; pick one from an adapter package such as `pqi-ffi` or `pqi-native`.

# v1.4.2.3

## Fixes

- Fix publishing.

# v1.4.2.2

## Fixes

- Fix pool capacity leak when `initSession` fails (#56)
- Fix background reaper never passively evicting idle connections: it used the connection-lifetime timeout instead of the idleness timeout when checking for idle connections

# v1.4.2.1

## Fixes

- Discard pooled connections after driver errors (#55)

# v1.4

- Migrated to `hasql-1.10`
- Updated connection settings API to use monoid-based `Settings` instead of list-based `[Setting]`
- Updated error types to use `Hasql.Errors` module instead of `Hasql.Session` and `Hasql.Connection`
- Changed session execution API from `Session.run session connection` to `Connection.use connection session`
- Error handling now uses `ConnectionSessionError` for connection-level issues instead of `ClientError`
- Updated statement construction to use `Statement.preparable` and `Statement.unpreparable` instead of direct constructor
- Hid the `Defaults` module from the public API

# v1.3

- Adapt to the new settings model of `hasql-1.9`

# v1.2

- Migrated to `hasql-1.7`
- Changed references to `QueryError` in observations to `SessionError`

# v1.1

- `ReadyForUseConnectionStatus` got extended with the `ConnectionReadyForUseReason` details.
- `initSession` setting added.

# v1

- Optional observability event stream added. Provides a flexible mechanism for monitoring the healthiness of the pool via logs and metrics.
- Configuration got isolated into a DSL, which will allow to provide new configurations without breaking backward compatibility.

# v0.10.1

- Avoid releasing connections on exceptions thrown in session

# v0.9

- Maximal lifetime added for connections. Allows to refresh the connections in time cleaning up the resources.

Breaking:

- The acquisition timeout is now non-optional.
- Moved to `DiffTime` for timeouts.

# v0.8.0.7

Fix excessive connections during releases due to race conditions.

# v0.8.0.5

Fix connections not returning to the pool on exceptions.

# v0.8.0.2

Fixed Windows build.

# v0.8

`release` became reusable. You can use it to destroy the whole pool (same as before), but now also you can use it to reset the connections.

Acquisition timeout added.

Breaking changes in API:

- Removed `PoolIsReleasedUsageError`
- `acquire` extended with the acquisition timeout parameter
- `acquireDynamically` extended with the acquisition timeout parameter

# v0.7.2

Added support for dynamic connection configuration ([issue #11](https://github.com/nikita-volkov/hasql-pool/issues/11)).

# v0.7.1.2

Fixed connections not being released if they were in use during the call to `release`.

# v0.7.1

Added `Exception` for `UsageError`.

# v0.7

Simplified the implementation a lot by removing the notion of timeout.

Breaking:
- Removed the `Settings` type
- Changed the signature of `acquire`

# v0.6

Moved away from "resource-pool" and fixed the handling of lost connections.

Breaking:

- Changed the suffix of `UsageError` constructors from `Error` to `UsageError`
- Added `PoolIsReleasedUsageError`
