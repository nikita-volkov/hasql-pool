-- Docs: https://hspec.github.io/hspec-discover.html
module Specs.BySubject.SpecHook where

import Hooks qualified
import Scripts qualified
import Test.Hspec
import Prelude

hook :: SpecWith Scripts.ScopeParams -> Spec
hook =
  aroundAll Hooks.postgres17 . parallel
