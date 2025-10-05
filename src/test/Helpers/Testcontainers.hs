module Helpers.Testcontainers where

import Data.Bool
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting.Connection
import Hasql.Connection.Setting.Connection.Param qualified as Connection.Setting.Connection.Param
import Test.Hspec qualified as Hspec
import TestcontainersPostgresql qualified
import Prelude

withConnectionSettings :: ([Connection.Setting.Setting] -> IO ()) -> IO ()
withConnectionSettings action = do
  TestcontainersPostgresql.run config \(host, port) -> do
    action
      [ Connection.Setting.connection
          ( Connection.Setting.Connection.params
              [ Connection.Setting.Connection.Param.host host,
                Connection.Setting.Connection.Param.port (fromIntegral port),
                Connection.Setting.Connection.Param.user "postgres",
                Connection.Setting.Connection.Param.password "",
                Connection.Setting.Connection.Param.dbname "postgres"
              ]
          )
      ]
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          distro = TestcontainersPostgresql.Distro17,
          auth = TestcontainersPostgresql.TrustAuth
        }

-- * Hspec

-- | Run spec with connection settings, providing an isolated environment for all tests.
aroundAllWithConnectionSettings ::
  Hspec.SpecWith [Connection.Setting.Setting] ->
  Hspec.Spec
aroundAllWithConnectionSettings spec =
  Hspec.aroundAll withConnectionSettings spec
