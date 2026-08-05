-- Docs: https://hspec.github.io/hspec-discover.html
module Specs.BySubject.SpecHook where

import Helpers.Hooks qualified as Hooks
import Helpers.Scripts qualified as Scripts
import Pqi qualified
import Test.Hspec

hook :: SpecWith Scripts.ScopeParams -> SpecWith Pqi.Adapter
hook hookedSpec =
  aroundAllWith
    (\action adapter -> Hooks.postgres17 \(host, port) -> action (adapter, host, port))
    (parallel hookedSpec)
