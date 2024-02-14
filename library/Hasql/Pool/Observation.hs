module Hasql.Pool.Observation where

import Hasql.Pool.Prelude

data Observation
  = ConnectionEstablishedObservation UUID
  | AttemptingToConnectObservation UUID
  | FailedToConnectObservation UUID (Maybe ByteString)
  | ConnectionReleasedObservation UUID ReleaseReason
  deriving (Show, Eq)

data ReleaseReason
  = AgingReleaseReason
  | IdlenessReleaseReason
  | TransportErrorReleaseReason (Maybe ByteString)
  | ReleaseActionCallReleaseReason
  deriving (Show, Eq)
