module Game.Update.Diplomacy
  ( diplomacy
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | When a player's ships are unopposed at a neutral base, that base's
-- friendliness (an argument to 'Neutral') goes up by one. At five the
-- base switches to the player's control.
diplomacy :: State Model ()
diplomacy =
  forOccupiedBases diplomacyAtPlace

diplomacyAtPlace :: PlaceId -> Base -> Player -> HashMap ShipId Ship -> State Model ()
diplomacyAtPlace placeId base occupyingPlayer _ =
  case baseOwner base of
    PlayerOwner _ ->
      pure ()

    Neutral oldFriendliness -> do
      let friendliness = 1 + fromMaybe 0 (HM.lookup occupyingPlayer oldFriendliness)
      if friendliness >= 5
        then
          adjustBase
            placeId
            (baseOwnerL .~ PlayerOwner occupyingPlayer)

        else
          adjustBase
            placeId
            (baseOwnerL . _Neutral %~ HM.insert occupyingPlayer friendliness)
