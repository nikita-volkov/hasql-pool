module Main where

import           Test.Tasty
import           Test.Tasty.Hspec

import           Hasql.Prelude
import qualified Hasql.PoolSpec

main = tests >>= defaultMain

tests :: IO TestTree
tests = testSpec "Hasql.Pool" Hasql.PoolSpec.spec