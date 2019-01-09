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

-- | __Player guide previous__: 'Game.Update.Travel.shipsEmbark'
--
-- The world contains diseases that can damage or destroy bases.
--
-- There's a slight chance of a disease outbreak at each base each turn.
-- When the base will have 'baseDisease' set to 'Latent' and you'll see
-- an indicator of this.
--
-- @Latent@ diseases have a small chance of spreading or getting worse each
-- turn. If they get worse they become 'Plague's, which have a high chance
-- of spreading or getting worse each turn.
--
-- When @Plague@s get worse they reduce the population at that base by one,
-- or if it is already at the minimum, have a slight chance of destroying it.
--
-- If one player has uncontested ships at a base any disease there is countered
-- in two ways:
--
-- 1. There's a chance of the disease being healed, down to @Latent@ if it was
-- a @Plague@, and healed entirely if it was @Latent@.
--
-- 2. Disease are less likely to spread to the base, or if already there
-- less likely to get worse. Unlike healing, this effect is stronger the more
-- ships there are present.
--
-- __Next__: 'ShipType'
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

-- | Whether diseases get worse or spread is calculated
-- based on the level of disease at the start of the turn.
--
-- To ensure this in the function above we freeze the places hashmap,
-- and use it to lookup bases which are passed into this function.
--
-- That way this function doesn't have to pull the bases out of the
-- of the 'State', potentially getting ones that have already had their
-- disease level raised this turn.
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
