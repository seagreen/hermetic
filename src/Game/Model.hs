{-# LANGUAGE TemplateHaskell #-}

-- | Types for the state of the game.
module Game.Model where

import qualified Data.Set as Set
import qualified Data.Text as T
import Game.Prelude
import Text.Read (readMaybe)

-- | The core game state.
data Model = Model
  { modelPlaces  :: HashMap PlaceId Place
  , modelShips   :: HashMap ShipId Ship
  , modelLog     :: Log
    -- ^ Records what combat occured this turn. Cleared at the start of each turn.
  , modelTurn    :: Nat
  , modelNextId  :: Nat
    -- ^ The supply for place and ship ids, which must be unique.
  , modelRandom  :: Gen
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

-- | The starting @Model@. Creates an empty @modelPlaces@ and @modelShips@.
-- These will be filled out by the a chosen scenario at the start of the game.
init :: Gen -> Model
init gen =
  Model
    { modelPlayers =
        Set.fromList
          [ Player1
          , Player2
          ]
    , modelPlaces = mempty
    , modelShips = mempty
    , modelLog = mempty
    , modelTurn = 1
    , modelNextId = 1
    , modelRandom = gen
    }

--------------------------------------------------------------------------------
-- * Places
--------------------------------------------------------------------------------

-- | Places are either bases or ruins.
-- These are the only locations ships can move to.
data Place = Place
  { placeName  :: Text
  , placePoint :: Point
  , placeSize  :: PlaceSize
  , placeType  :: PlaceType
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Invariant: unique per game.
newtype PlaceId
  = PlaceId Nat
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | Affects how large a base at this place can grow.
--
-- Fixed for the duration of the game.
data PlaceSize
  = Small
  | Medium
  | Large
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | Each place is either a base or a ruin.
data PlaceType
  = PBase Base
  | Ruin
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | Each player starts with at least one base and loses when they don't have any.
--
-- Provides economic production and defense.
data Base = Base
  { baseOwner         :: Owner
  , basePopulation    :: Population
  , baseDisease       :: Disease
  , baseInstallations :: Set Installation
  , baseShields       :: Nat
  , baseBuilding      :: BuildOrder
  , baseInProgress    :: HashMap BuildOrder Double
    -- ^ Tracks progress for each build order. Production only goes towards
    -- the build order in @baseBuilding@, but since you can switch what that
    -- is this also tracks partially completed build orders.
    --
    -- Note that switching build orders wastes a turn of production
    -- while the economy re-tools.
    --
    -- See "Game.Update.Build" for details.
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Bases are either owned by a player or neutral.
data Owner
  = PlayerOwner Player
  | Neutral (HashMap Player Nat)
    -- ^ The HashMap is the friendliness of the neutral base towards the players.
    -- It starts at 0 for each player.
    --
    -- When a player is the only one with ships at a neutral base, it ticks
    -- up by one for them. When it reaches 5 the base switches allegiance to them.
    --
    -- See "Game.Update.Diplomacy" for details.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | Population can only grow to a Megacity or Ecumenopolis when the host
-- Place has a large enough 'PlaceSize'. So we don't derive 'Enum' or
-- 'Bounded' for it because the behavior of 'maxBound' and 'nextBounded' will be
-- incorrect.
--
-- Instead we define @minPop@, @prevPop@ and @incrementPop@
-- in "Game.Update.Shared".
data Population
  = Outpost
  | Settlement
  | City
  | Megacity
  | Ecumenopolis
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | See "Game.Update.Disease" for details.
data Disease
  = Healthy
  | Latent
  | Plague
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON)

data Installation
  = Shield
  | Booster
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data BuildOrder
  = BuildPopulation
  | BuildShip ShipType
  | BuildShield
  | BuildBooster
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, ToJSONKey, FromJSON)

--------------------------------------------------------------------------------
-- * Ships
--------------------------------------------------------------------------------

data Ship = Ship
  { shipPlayer :: Player
  , shipLocation :: ShipLocation
  , shipType :: ShipType
  , shipShields :: Bool
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Invariant: unique per game.
newtype ShipId
  = ShipId Nat
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data ShipLocation
  = AtBase PlaceId
  | InFlight Point PlaceId IsBoosted
    -- ^ Currently at Point, flying to PlaceId
  | Destroyed
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

data IsBoosted
  = Boosted
  | NotBoosted
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

data ShipType
  = Corvette
    -- ^ Standard ship.
  | Station
  | Monitor
    -- ^ A more expensive, bombardment focused ship.
    --
    -- If bombarding and a 'Shield' installation is present, it's destroyed.
    -- If bombarding and a 'Shield' isn't present, the base is destroyed.
    -- Also has x3 weapons during combat.
  deriving stock (Eq, Ord, Show, Generic)
  -- Order matters for the derived Ord instance since we use it to
  -- show more important ships in the UI first
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- * Other
--------------------------------------------------------------------------------

data Player
  = Player1
  | Player2
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, ToJSONKey, FromJSON)

data Log = Log
  { logCombat :: HashMap PlaceId (Set ShipId)
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

instance Semigroup Log where
  Log a1 <> Log a2 = Log (a1 <> a2)

instance Monoid Log where
  mempty = Log mempty

newtype Gen
  = Gen { unGen :: StdGen }
  deriving (Show)

instance Eq Gen where
  -- | Needed for testing invertability through JSON.
  Gen a == Gen b = show a == show b

instance ToJSON Gen where
  toJSON (Gen stdGen) =
    String (T.pack (show stdGen))

instance FromJSON Gen where
  parseJSON = withText "Gen" $ \t ->
    case readMaybe (T.unpack t) of
      Nothing -> fail ("Couldn't parse Gen: " <> T.unpack t)
      Just stdGen -> pure (Gen stdGen)

data Orders = Orders
  { ordersBuild :: HashMap PlaceId BuildOrder
  , ordersEmbark :: HashMap ShipId PlaceId
  } deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Semigroup Orders where
  Orders a1 b1 <> Orders a2 b2 = Orders (a1 <> a2) (b1 <> b2)

instance Monoid Orders where
  mempty = Orders mempty mempty

-- * Lenses
mkLenses ''Base
mkLenses ''Log
mkLenses ''Model
mkLenses ''Place
mkLenses ''Ship
mkLenses ''Orders

makePrisms ''Owner
makePrisms ''PlaceType
