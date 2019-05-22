module Main where

import BasePrelude
import Test.Hspec
import Hasql.Pool
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement


main = hspec $ do
  describe "Hasql.Pool.use" $ do
    it "releases a spot in the pool when there is an error" $ do
      pool <- acquire (1, 1, 1, "host=localhost port=5432 user=postgres dbname=postgres")
      let
        statement = Statement.Statement "" Encoders.unit Decoders.unit True
        session = Session.statement () statement
        in do
          use pool session `shouldNotReturn` (Right ())
      let
        session = let
          statement = let
            decoder = Decoders.singleRow (Decoders.column Decoders.int8)
            in Statement.Statement "SELECT 1" Encoders.unit decoder True
          in Session.statement () statement
        in do
          use pool session `shouldReturn` (Right 1)
