module Helpers.Testcontainers where

import Data.Bool
import Data.Text (Text)
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
    action (mkConnectionSettings host port Nothing)
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          distro = TestcontainersPostgresql.Distro17,
          auth = TestcontainersPostgresql.TrustAuth
        }

-- | Provides both base connection settings and a function to create tagged versions
withConnectionSettingsEx :: (([Connection.Setting.Setting], Text -> [Connection.Setting.Setting]) -> IO ()) -> IO ()
withConnectionSettingsEx action = do
  TestcontainersPostgresql.run config \(host :: Text, port :: Int) -> do
    let baseSettings = mkConnectionSettings host port Nothing
        makeTaggedSettings appName = mkConnectionSettings host port (Just appName)
    action (baseSettings, makeTaggedSettings)
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          distro = TestcontainersPostgresql.Distro17,
          auth = TestcontainersPostgresql.TrustAuth
        }

-- | Create connection settings for the given host/port, optionally with an application name
mkConnectionSettings :: Text -> Int -> Maybe Text -> [Connection.Setting.Setting]
mkConnectionSettings host port maybeAppName =
  let baseParams =
        [ Connection.Setting.Connection.Param.host host,
          Connection.Setting.Connection.Param.port (fromIntegral port),
          Connection.Setting.Connection.Param.user "postgres",
          Connection.Setting.Connection.Param.password "",
          Connection.Setting.Connection.Param.dbname "postgres"
        ]
      allParams = case maybeAppName of
        Nothing -> baseParams
        Just appName -> baseParams ++ [Connection.Setting.Connection.Param.other "application_name" appName]
   in [Connection.Setting.connection (Connection.Setting.Connection.params allParams)]

withConnectionSettingsAndAppName :: Text -> (([Connection.Setting.Setting], Text) -> IO ()) -> IO ()
withConnectionSettingsAndAppName appName action = do
  TestcontainersPostgresql.run config \(host :: Text, port :: Int) -> do
    action (mkConnectionSettings host port (Just appName), appName)
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

-- | Run spec with connection settings and a function to create tagged settings
aroundAllWithConnectionSettingsEx ::
  Hspec.SpecWith ([Connection.Setting.Setting], Text -> [Connection.Setting.Setting]) ->
  Hspec.Spec
aroundAllWithConnectionSettingsEx spec =
  Hspec.aroundAll withConnectionSettingsEx spec
