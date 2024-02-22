module Hasql.Pool.Observation where

import Hasql.Pool.Prelude

data Observation
  = ConnectionObservation
      -- | Generated connection ID.
      -- For grouping the observations by one connection.
      UUID
      -- | Connection status that it has entered.
      ConnectionStatus
  deriving (Show, Eq)

data ConnectionStatus
  = -- | Connection is being established.
    ConnectingConnectionStatus
  | -- | Connection is established and not occupied.
    ReadyForUseConnectionStatus
  | -- | Is being used by some session.
    --
    -- After it's done the status will transition to 'ReadyForUseConnectionStatus' or 'ReleasedConnectionStatus'.
    InUseConnectionStatus
  | -- | Connection terminated.
    TerminatedConnectionStatus ConnectionTerminationReason
  deriving (Show, Eq)

data ConnectionTerminationReason
  = -- | The age timeout of the connection has passed.
    AgingConnectionTerminationReason
  | -- | The timeout of how long a connection may remain idle in the pool has passed.
    IdlenessConnectionTerminationReason
  | -- | Connectivity issues with the server.
    NetworkErrorConnectionTerminationReason (Maybe Text)
  | -- | User has invoked the 'Hasql.Pool.release' procedure.
    ReleaseConnectionTerminationReason
  deriving (Show, Eq)
