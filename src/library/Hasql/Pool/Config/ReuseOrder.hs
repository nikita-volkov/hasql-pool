module Hasql.Pool.Config.ReuseOrder where

import Hasql.Pool.Prelude

-- | Which of the idle connections a pool request gets.
data ReuseOrder
  = -- | Last in, first out: the one returned most recently.
    --
    -- The pool keeps reusing the few connections the load needs, so the rest
    -- time out as idle, and the ones in use keep their caches warm.
    LIFO
  | -- | First in, first out: the one returned longest ago.
    --
    -- Pool requests cycle through all the connections, so steady load keeps
    -- every connection open.
    FIFO
  deriving (Show, Eq)
