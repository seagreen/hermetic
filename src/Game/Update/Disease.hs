module Game.Update.Disease
  ( diseaseSpread
  , shipsHeal
  ) where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Game.Model
import Game.Prelude
import Game.Update.Shared

-- | Whether diseases get worse or spread is calculated
-- based on the level of disease at the start of the turn.
--
-- To ensure this we freeze the places hashmap, and use it to lookup
-- bases to pass into @spreadCheck@. That way @spreadCheck@
-- doesn't use the bases which 'forBasesWithControlStatus' pulls out
-- of the 'State', potentially getting ones that have already had their
-- disease level raised this turn.
diseaseSpread :: State Model ()
diseaseSpread = do
  frozenPlaces <- use modelPlacesL
  forBasesWithControlStatus (f frozenPlaces)
  where
    f :: HashMap PlaceId Place
      -> PlaceId
      -> Base
      -> Maybe (Player, HashMap ShipId Ship)
      -> State Model ()
    f frozenPlaces placeId _ mController =
      case placeType (getPlace placeId frozenPlaces) of
        Ruin ->
          pure ()

        PBase base ->
          diseaseAtBase placeId base mController

diseaseAtBase
  :: PlaceId
  -> Base
  -> Maybe (Player, HashMap ShipId Ship)
  -> State Model ()
diseaseAtBase placeId base mController = do
  case baseDisease base of
    Healthy ->
      runDeNovo

    Latent -> do
      runSpread (shipModifier 0.05)
      runWorsen (shipModifier 0.07)

    Plague -> do
      runSpread (shipModifier 0.4)
      runWorsen (shipModifier 0.5)
  where
    runDeNovo :: State Model ()
    runDeNovo = do
      started <- runRandom $ probability (shipModifier 0.01)
      when started $ adjustBase placeId (baseDiseaseL .~ Latent)

    runSpread :: Double -> State Model ()
    runSpread chance = do
      didSpread <- runRandom $ probability chance
      when didSpread $ do
        places <- use modelPlacesL
        res <- runRandom (frontWeightedChoice (sortByClosest placeId places))
        case res of
          Nothing ->
            pure ()

          Just target ->
            worsen target

    runWorsen :: Double -> State Model ()
    runWorsen chance = do
      didWorsenHere <- runRandom $ probability chance
      when didWorsenHere (worsen placeId)

    -- If exactly one player has ships orbiting a base, the chance of disease
    -- effects is lessened.
    shipModifier :: Double -> Double
    shipModifier n =
      case mController of
        Nothing ->
          n

        Just (_, controllingShips) ->
          repeatedlyApply (* 0.5) (fromIntegral (HM.size controllingShips)) n

sortByClosest :: PlaceId -> HashMap PlaceId Place -> [PlaceId]
sortByClosest placeId places =
  map fst . List.sortOn f . HM.toList . HM.delete placeId $ places
  where
    place = getPlace placeId places

    f :: (PlaceId, Place) -> Float
    f (_,a) =
      distance (placePoint place) (placePoint a)

worsen :: PlaceId -> State Model ()
worsen placeId = do
  places <- use modelPlacesL
  let place = getPlace placeId places
  case placeType place of
    Ruin ->
      pure ()

    PBase base ->
      if baseDisease base /= maxBound
        then
          adjustBase placeId (baseDiseaseL %~ nextBounded)

        else
          if basePopulation base /= minPop
            then
              adjustBase placeId (basePopulationL %~ prevPop)
            else do
              fallIntoRuin <- runRandom (probability 0.05)
              when fallIntoRuin $
                modelPlacesL %= HM.adjust (placeTypeL .~ Ruin) placeId

-- | When a players ships are present at a base unopposed they have a chance
-- of healing any disease there.
shipsHeal :: State Model ()
shipsHeal =
  forControlledBases healCheck

healCheck :: PlaceId -> Base -> Player -> HashMap ShipId Ship -> State Model ()
healCheck placeId base _ _ = do
  case baseDisease base of
    Healthy ->
      pure ()

    Latent ->
      attemptHeal

    Plague ->
      attemptHeal
  where
    attemptHeal :: State Model ()
    attemptHeal = do
      b <- runRandom (probability 0.3)
      when b $ adjustBase placeId (baseDiseaseL %~ prevBounded)
