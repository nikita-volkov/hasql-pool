# 0.9

Connections now have a configurable maximal lifetime, configurable via a new extensible configuration interface. The pool is now actively managed, and is created using a combinator that takes care to free resources after use.

Breaking changes in API:

- `acquire` and `acquireDynamically` are replaced with `withPool` and `withPoolConf`

# 0.8.0.7

Fix excessive connections during releases due to race conditions.

# 0.8.0.5

Fix connections not returning to the pool on exceptions.

# 0.8.0.2

Fixed Windows build.

# 0.8

`release` became reusable. You can use it to destroy the whole pool (same as before), but now also you can use it to reset the connections.

Acquisition timeout added.

Breaking changes in API:

- Removed `PoolIsReleasedUsageError`
- `acquire` extended with the acquisition timeout parameter
- `acquireDynamically` extended with the acquisition timeout parameter

# 0.7.2

Added support for dynamic connection configuration ([issue #11](https://github.com/nikita-volkov/hasql-pool/issues/11)).

# 0.7.1.2

Fixed connections not being released if they were in use during the call to `release`.

# 0.7.1

Added `Exception` for `UsageError`.

# 0.7

Simplified the implementation a lot by removing the notion of timeout.

Breaking:
- Removed the `Settings` type
- Changed the signature of `acquire`

# 0.6

Moved away from "resource-pool" and fixed the handling of lost connections.

Breaking:

- Changed the suffix of `UsageError` constructors from `Error` to `UsageError`
- Added `PoolIsReleasedUsageError`
