-- Docs: https://hspec.github.io/hspec-discover.html
module Specs.BySubject.SpecHook where

import Helpers.Hooks qualified as Hooks
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

hook :: SpecWith Scripts.ScopeParams -> Spec
hook =
  aroundAll Hooks.postgres17 . parallel
