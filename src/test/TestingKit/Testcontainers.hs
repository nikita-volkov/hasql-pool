module TestingKit.Testcontainers where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Text qualified as Text
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

-- | Get connection settings as text for use with legacy string-based configuration
getConnectionSettingsText :: IO Text.Text
getConnectionSettingsText = do
  settingsRef <- newIORef Nothing
  TestcontainersPostgresql.run config \(host, port) -> do
    let connectionText = 
          Text.unwords
            [ "host=" <> host,
              "port=" <> Text.pack (show port),
              "user=postgres",
              "password=",
              "dbname=postgres"
            ]
    writeIORef settingsRef (Just connectionText)
  readIORef settingsRef >>= maybe (fail "Failed to get connection settings") return
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
