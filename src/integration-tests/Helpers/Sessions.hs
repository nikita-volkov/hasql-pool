module Helpers.Sessions
  ( selectOne,
    badQuery,
    closeConn,
    setSetting,
    getSetting,
    countConnections,
  )
where

import Data.Tuple.All
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Prelude

selectOne :: Session.Session Int64
selectOne =
  Session.statement () statement
  where
    statement = Statement.Statement "SELECT 1" Encoders.noParams decoder True
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

badQuery :: Session.Session ()
badQuery =
  Session.statement () statement
  where
    statement = Statement.Statement "zzz" Encoders.noParams Decoders.noResult True

closeConn :: Session.Session ()
closeConn = do
  conn <- ask
  liftIO $ Connection.release conn

setSetting :: Text -> Text -> Session.Session ()
setSetting name value = do
  Session.statement (name, value) statement
  where
    statement = Statement.Statement "SELECT set_config($1, $2, false)" encoder Decoders.noResult True
    encoder =
      mconcat
        [ sel1 >$< Encoders.param (Encoders.nonNullable Encoders.text),
          sel2 >$< Encoders.param (Encoders.nonNullable Encoders.text)
        ]

getSetting :: Text -> Session.Session (Maybe Text)
getSetting name = do
  Session.statement name statement
  where
    statement = Statement.Statement "SELECT current_setting($1, true)" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text))

countConnections :: Text -> Session.Session Int64
countConnections appName = do
  Session.statement appName statement
  where
    statement = Statement.Statement "SELECT count(*) FROM pg_stat_activity WHERE application_name = $1" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
