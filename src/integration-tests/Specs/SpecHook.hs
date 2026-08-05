-- Docs: https://hspec.github.io/hspec-discover.html
module Specs.SpecHook where

import Helpers.Adapters qualified as Adapters
import Pqi qualified
import Test.Hspec

hook :: SpecWith Pqi.Adapter -> Spec
hook = Adapters.hook
