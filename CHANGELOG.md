# Unreleased (PostgREST fork)

Added support for flushing the pool without destroying it ([PR #2](https://github.com/PostgREST/hasql-pool/pull/2)).

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
