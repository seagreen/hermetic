-- | Just re-exports.
module Game
  ( -- * Good starting points for reading the code
    module Game.Model
  , module Game.Update

    -- * Other re-exports
  , module X
  ) where

import Game.Model
import Game.Update

import Game.Outcome as X
import Game.Update.Build as X hiding (build)
import Game.Update.Combat as X
import Game.Update.Diplomacy as X
import Game.Update.Disease as X
import Game.Update.Shared as X
import Game.Update.Travel as X
