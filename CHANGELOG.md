# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0] - 2021-10-13

Slight breaking change in the API, see `Changed`.

## Changed

- `Settings` is now a record.

  `defaultSettings` can be used to be forward-compatible in case new fields get added in the future.
  
## Fixed

- Broken connections will be dropped by default. See below.

## Added
  
- `onQueryError` added to the settings.

  Will run on every query error returned by the database, and decide whether to drop the connection.
  
  The `defaultOnQueryError` drops the connection on a hasql `ClientError`.

## Improved

- Documentation restructured & extended in places.

#  [0.5.2] - 2020-05-15

## Fixed

- Destroy connection on session exception

  If the `Session a` throws an IO-based exception, we now destroy the connection resource.

# [0.5.1] - 2019-05-28

## Added

- `Show` instance for `Pool`

# [Older Versions]

For older versions, see the git history.
