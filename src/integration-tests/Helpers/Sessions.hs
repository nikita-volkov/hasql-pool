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
import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Prelude

selectOne :: Session.Session Int64
selectOne =
  Session.statement () statement
  where
    statement = Statement.preparable "SELECT 1::int8" Encoders.noParams decoder
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

badQuery :: Session.Session ()
badQuery =
  Session.statement () statement
  where
    statement =
      Statement.preparable "zzz" Encoders.noParams Decoders.noResult

closeConn :: Session.Session ()
closeConn =
  Session.onLibpqConnection \conn -> do
    Pq.finish conn
    pure (Right (), conn)

setSetting :: Text -> Text -> Session.Session ()
setSetting name value = do
  Session.statement (name, value) statement
  where
    statement =
      Statement.preparable "SELECT set_config($1, $2, false)" encoder Decoders.noResult
    encoder =
      mconcat
        [ sel1 >$< Encoders.param (Encoders.nonNullable Encoders.text),
          sel2 >$< Encoders.param (Encoders.nonNullable Encoders.text)
        ]

getSetting :: Text -> Session.Session (Maybe Text)
getSetting name = do
  Session.statement name statement
  where
    statement = Statement.preparable "SELECT current_setting($1, true)" encoder decoder
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text))

countConnections :: Text -> Session.Session Int64
countConnections appName = do
  Session.statement appName statement
  where
    statement = Statement.preparable "SELECT count(*) FROM pg_stat_activity WHERE application_name = $1" encoder decoder
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
