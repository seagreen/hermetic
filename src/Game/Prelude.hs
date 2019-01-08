{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Re-exports hidden. They include a bunch of stuff from "Prelude",
-- other parts of @base@, @containers@, etc. See source for details.
module Game.Prelude
  ( module Game.Prelude
  , module X
  ) where

import Control.Category as X ((>>>))
import Control.Lens as X hiding (Zoom, index, view, zoom)
import Control.Monad as X hiding (fmap)
import Control.Monad.Random as X hiding (next)
import Data.Aeson as X hiding ((.=))
import Data.Foldable as X
import Data.Hashable as X (Hashable)
import Data.HashMap.Strict as X (HashMap)
import Data.Map.Strict as X (Map)
import Data.Maybe as X
import Data.Ord as X hiding (Down)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Debug.Trace as X
import GHC.Generics as X (Generic)
import Graphics.Gloss.Interface.IO.Game as X (Point)
import Prelude as X hiding (fmap, id, init, map, pred, succ)
import Safe.Foldable as X (maximumByMay, minimumByMay)

import qualified Control.Monad.Random as MonadRandom
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Haskell.TH
import qualified Numeric
import qualified Numeric.Natural
import qualified Prelude
import qualified Safe

type Nat = Numeric.Natural.Natural

atMostDecimals :: Nat -> Double -> Text
atMostDecimals n d =
    T.dropWhileEnd (=='.')
  . T.dropWhileEnd (== '0')
  . T.pack
  $ Numeric.showFFloat (Just (fromIntegral n)) d ""

enumerateAll :: (Enum a, Bounded a, Ord a) => Set a
enumerateAll =
  Set.fromList [minBound .. maxBound]

reverseOrdering :: Ordering -> Ordering
reverseOrdering = \case
  LT -> GT
  EQ -> EQ
  GT -> LT

-- | A replacement for 'HM.toList' where the result order is sorted
-- by key instead of being left unspecified.
hmToList :: Ord k => HashMap k v -> [(k,v)]
hmToList =
  List.sortOn fst . HM.toList

hmTraverseWithKey_ :: Applicative f => (k -> v1 -> f ()) -> HashMap k v1 -> f ()
hmTraverseWithKey_ f =
  traverse_ (uncurry f) . HM.toList

identity :: a -> a
identity =
  Prelude.id

map :: Functor f => (a -> b) -> f a -> f b
map =
  Prelude.fmap

mapWithIndex :: (Nat -> a -> b) -> [a] -> [b]
mapWithIndex f =
  zipWith f [0..]

-- | Make lenses suffixed with "L"
-- (from https://stackoverflow.com/a/26563262)
mkLenses :: Name -> DecsQ
mkLenses =
  makeLensesWith appendL
  where
    appendL :: LensRules
    appendL =
      lensRules
        & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]

repeatedlyApply :: (a -> a) -> Nat -> a -> a
repeatedlyApply f n a =
  iterate f a !! (fromIntegral n)

repeatedlyApplyM :: forall a m. Monad m => (a -> m a) -> Nat -> a -> m a
repeatedlyApplyM f n start =
  foldM g start ns
  where
    ns :: [()]
    ns =
      replicate (fromIntegral n) ()

    g :: a -> () -> m a
    g a () =
      f a

-- | These rely on Enum and Bounded behaving as expected.
prevBounded :: (Enum a, Eq a, Bounded a) => a -> a
prevBounded =
  Safe.predSafe

nextBounded :: (Enum a, Eq a, Bounded a) => a -> a
nextBounded =
  Safe.succSafe

--------------------------------------------------------------------------------
-- * Randomness

probability
  :: MonadRandom m
  => Double -- ^ From 0 to 1.
  -> m Bool
probability n = do
  res <- MonadRandom.getRandomR (0, 1)
  pure (res <= n)

frontWeightedChoice :: forall a m. MonadRandom m => [a] -> m (Maybe a)
frontWeightedChoice xs =
  MonadRandom.weightedMay (fst (foldl' f ([], 1) xs))
  where
    f :: ([(a, Rational)], Double) -> a -> ([(a, Rational)], Double)
    f (ys, n) y =
      ( (y, toRational n) : ys
      , n * decreaseBy
      )

    decreaseBy :: Double
    decreaseBy =
      2/3

-- | Given some number of "hits" spread them out over a collection of targets.
distributeHits
  :: forall m k a.
     (MonadRandom m, Eq k, Hashable k)
  => (a -> Maybe a)
     -- ^ When a target is "hit", determine whether it can be hit any more.
     --
     -- @Just@ means it can (e.g. it had shields and so wasn't destroyed).
     --
     -- @Nothing@ means it cannot (e.g. it was destroyed).
  -> HashMap k a
     -- ^ Targets
  -> Nat
     -- ^ Number of hits
  -> m (HashMap k a, HashMap k a)
     -- ^ The first part of the tuple is destoyed targets, the second is not destroyed.
distributeHits resultOfHit targets numHits =
  repeatedlyApplyM f numHits (mempty, targets)
  where
    f :: (HashMap k a, HashMap k a) -> m (HashMap k a, HashMap k a)
    f (destroyedSoFar, currentTargets) = do
      mTarget <- MonadRandom.uniformMay (HM.toList currentTargets)
      pure $
        case mTarget of
          Nothing ->
            (destroyedSoFar, currentTargets)

          Just (id, target) ->
            case resultOfHit target of
              Nothing ->
                ( HM.insert id target destroyedSoFar
                , HM.delete id currentTargets
                )

              Just updatedTarget ->
                ( destroyedSoFar
                , HM.insert id updatedTarget currentTargets
                )

--------------------------------------------------------------------------------
-- * Math

data Rectangle = Rectangle
  { rectangleOrigin :: Point
  , rectangleWidth :: Float
  , rectangleHeight :: Float
  } deriving (Eq, Ord, Show, Generic)

data Box = Box
  { boxWidth :: Float
  , boxHeight :: Float
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)
    -- ToJSON needed because used by the model for screen size

newtype Radius
  = Radius { unRadius :: Float }
  deriving (Eq, Ord, Show)

-- | Given a point, if it's not in a certain rectangle return nothing.
--
-- If it is in the rectangle, transform its coordinates to be
-- relative to the center of the rectangle.
rectangleCoordinates :: Point -> Rectangle -> Maybe Point
rectangleCoordinates (x,y) (Rectangle (centerX,centerY) width height) = do
  guard (x >= centerX - width / 2)
  guard (x <= centerX + width / 2)
  guard (y >= centerY - height / 2)
  guard (y <= centerY + height / 2)
  Just (x - centerX , y - centerY)

angleBetweenPoints :: Point -> Point -> Float
angleBetweenPoints (x1,y1) (x2,y2) =
  atan2 (y2 - y1) (x2 - x1)

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) =
  sqrt ((x1 - x2)^(2 :: Int) + (y1 - y2)^(2 :: Int))

deltas :: Float -> Float -> (Float, Float)
deltas angle speed =
  ( speed * cos angle
  , speed * sin angle
  )

toDegrees :: Float -> Float
toDegrees n =
  n * 180 / pi
