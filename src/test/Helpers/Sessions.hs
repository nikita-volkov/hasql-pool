module Helpers.Sessions
  ( selectOneSession,
    badQuerySession,
    closeConnSession,
    setSettingSession,
    getSettingSession,
    countConnectionsSession,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Text (Text)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Prelude

selectOneSession :: Session.Session Int64
selectOneSession =
  Session.statement () statement
  where
    statement = Statement.Statement "SELECT 1" Encoders.noParams decoder True
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

badQuerySession :: Session.Session ()
badQuerySession =
  Session.statement () statement
  where
    statement = Statement.Statement "zzz" Encoders.noParams Decoders.noResult True

closeConnSession :: Session.Session ()
closeConnSession = do
  conn <- ask
  liftIO $ Connection.release conn

setSettingSession :: Text -> Text -> Session.Session ()
setSettingSession name value = do
  Session.statement (name, value) statement
  where
    statement = Statement.Statement "SELECT set_config($1, $2, false)" encoder Decoders.noResult True
    encoder =
      contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
        <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text))

getSettingSession :: Text -> Session.Session (Maybe Text)
getSettingSession name = do
  Session.statement name statement
  where
    statement = Statement.Statement "SELECT current_setting($1, true)" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text))

countConnectionsSession :: Text -> Session.Session Int64
countConnectionsSession appName = do
  Session.statement appName statement
  where
    statement = Statement.Statement "SELECT count(*) FROM pg_stat_activity WHERE application_name = $1" encoder decoder True
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
