-- | Hooks for Hspec.
module Helpers.Hooks where

import Data.Bool
import TestcontainersPostgresql qualified
import Prelude hiding (Handler)

-- | Testing action in the scope of the host name and port of a running fresh isolated postgres server.
type Handler = (Text, Word16) -> IO ()

postgres17 :: Handler -> IO ()
postgres17 handler =
  TestcontainersPostgresql.run
    TestcontainersPostgresql.Config
      { forwardLogs = False,
        tagName = "postgres:17",
        auth = TestcontainersPostgresql.TrustAuth
      }
    (\(host, portInt) -> handler (host, fromIntegral portInt))
