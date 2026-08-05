# v1.3.0.5

## Fixes

- Fix pool capacity leak when `initSession` fails (#56)
- Fix background reaper never passively evicting idle connections: it used the connection-lifetime timeout instead of the idleness timeout when checking for idle connections

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
