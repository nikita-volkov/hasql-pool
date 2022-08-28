module GenericPool
  ( Pool,
  )
where

import qualified BoundedCounter
import PowerPrelude
import qualified TimeExtras.IO as TimeExtrasIO

-- | Pool of connections to DB.
data Pool acqErr handle = Pool
  { -- | Connection timeout.
    poolTimeout :: Int,
    poolAcquirer :: IO (Either acqErr handle),
    -- | Queue of established connections.
    poolSlotQueue :: TQueue (Slot handle),
    poolFreeSlotCounter :: BoundedCounter.BoundedCounter,
    -- | Timestamp of the last release. Checked by connections in use to determine, whether they should be released.
    poolReleaseTimestamp :: TVar Int
  }

-- | Available connection.
data Slot handle = Slot
  { activeSlotLastUseTimestamp :: Int,
    activeSlotHandle :: handle
  }

use :: Pool acqErr handle -> (handle -> IO (res, Bool)) -> IO (Either acqErr res)
use Pool {..} use =
  join . atomically $
    error "TODO"
