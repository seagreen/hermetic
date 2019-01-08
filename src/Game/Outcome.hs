{-# LANGUAGE TemplateHaskell #-}

module Game.Outcome where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game.Model
import Game.Prelude

data Outcome
  = Victor Player
  | AllDefeated
  | Ongoing
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

outcome :: Model -> Outcome
outcome Model{..} =
  case mapMaybe hasBase (Set.toList enumerateAll :: [Player]) of
    [] ->
      AllDefeated

    [player] ->
      Victor player

    _ ->
      Ongoing
  where
    hasBase :: Player -> Maybe Player
    hasBase player =
      case mapMaybe (friendly player) (HM.elems modelPlaces) of
        [] -> Nothing
        _ -> Just player

    friendly :: Player -> Place -> Maybe Base
    friendly player place = do
      base <- case placeType place of
                Ruin ->
                  Nothing

                PBase base ->
                  Just base

      case baseOwner base of
        Neutral _ ->
          Nothing

        PlayerOwner basePlayer ->
          if basePlayer == player
            then Just base
            else Nothing

-- * Lenses
makePrisms ''Outcome
