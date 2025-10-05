module Helpers.Scripts where

import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting.Connection
import Hasql.Connection.Setting.Connection.Param qualified as Connection.Setting.Connection.Param
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Config
import System.Random.Stateful qualified as Random
import TextBuilder qualified
import Prelude

-- |
-- Parameters provided by the scope.
-- Host and port of a running isolated postgres server.
type ScopeParams = (Text, Word16)

onTaggedPool :: Int -> DiffTime -> DiffTime -> DiffTime -> Text -> ScopeParams -> (Pool.Pool -> IO ()) -> IO ()
onTaggedPool poolSize acqTimeout maxLifetime maxIdletime appName (host, port) =
  bracket
    ( Pool.acquire
        ( Config.settings
            [ Config.size poolSize,
              Config.acquisitionTimeout acqTimeout,
              Config.agingTimeout maxLifetime,
              Config.idlenessTimeout maxIdletime,
              Config.staticConnectionSettings
                [ Connection.Setting.connection
                    ( Connection.Setting.Connection.params
                        [ Connection.Setting.Connection.Param.host host,
                          Connection.Setting.Connection.Param.port (fromIntegral port),
                          Connection.Setting.Connection.Param.user "postgres",
                          Connection.Setting.Connection.Param.password "",
                          Connection.Setting.Connection.Param.dbname "postgres",
                          Connection.Setting.Connection.Param.other "application_name" appName
                        ]
                    )
                ]
            ]
        )
    )
    Pool.release

onAutotaggedPool :: Int -> DiffTime -> DiffTime -> DiffTime -> ScopeParams -> (Text -> Pool.Pool -> IO ()) -> IO ()
onAutotaggedPool poolSize acqTimeout maxLifetime maxIdletime (host, port) cont = do
  -- Generate app name
  appName <- generateName "hasql-pool-test-"
  onTaggedPool poolSize acqTimeout maxLifetime maxIdletime appName (host, port) (cont appName)

onDefaultTaggedPool :: ScopeParams -> (Text -> Pool.Pool -> IO ()) -> IO ()
onDefaultTaggedPool =
  onAutotaggedPool 3 10 1_800 1_800

generateName :: Text -> IO Text
generateName prefix = do
  uniqueNum1 <- Random.uniformWord64 Random.globalStdGen
  uniqueNum2 <- Random.uniformWord64 Random.globalStdGen
  pure
    $ TextBuilder.toText
    $ mconcat
    $ [ TextBuilder.text prefix,
        TextBuilder.decimal uniqueNum1,
        TextBuilder.decimal uniqueNum2
      ]

generateVarname :: IO Text
generateVarname = generateName "testing.v"
