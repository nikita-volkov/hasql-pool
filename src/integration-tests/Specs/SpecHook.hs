-- Docs: https://hspec.github.io/hspec-discover.html
module Specs.SpecHook where

import Helpers.Adapters qualified as Adapters
import Helpers.Hooks qualified as Hooks
import Helpers.Scripts qualified as Scripts
import Test.Hspec

hook :: SpecWith Scripts.ScopeParams -> Spec
hook hookedSpec =
  Adapters.hook
    ( aroundAllWith
        (\action adapter -> Hooks.postgres17 \(host, port) -> action (adapter, host, port))
        (parallel hookedSpec)
    )
