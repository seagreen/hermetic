module Scenario.Tannen where

import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Game
import Game.Prelude

fillBoard :: State Model ()
fillBoard = do
  tannen <- newPlace $ def (0,300) "Tannen" & placeSizeL .~ Large
                                            & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player1
                                            & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                            & placeTypeL . _PBase . baseShieldsL .~ startingShields
  _ <- newPlace $ def (-300,400) "Varad"
  _ <- newPlace $ def (300,400) "Cartago"

  renga <- newPlace $ def (0,-300) "Renga" & placeSizeL .~ Large
                                           & placeTypeL . _PBase . baseOwnerL .~ PlayerOwner Player2
                                           & placeTypeL . _PBase . baseInstallationsL %~ Set.insert Shield
                                           & placeTypeL . _PBase . baseShieldsL .~ startingShields
  _ <- newPlace $ def (-300,-400) "Mugat"
  _ <- newPlace $ def (300,-400) "Nakana"

  _ <- newPlace $ def (500,0) "K Station" & placeSizeL .~ Small
  _ <- newPlace $ def (800,0) "Atalanta" & placeSizeL .~ Large
                                         & placeTypeL . _PBase . basePopulationL .~ Settlement
  _ <- newPlace $ def (1000,300) "Firbeck"
  _ <- newPlace $ def (1000,-300) "Banting"

  _ <- newPlace $ def (-1500,0) "Terminal" & placeSizeL .~ Small

  _ <- newShip $ defShip Player1 (InFlight (0,0) tannen NotBoosted)
  _ <- newShip $ defShip Player2 (InFlight (0,0) renga NotBoosted)

  -- For testing ship types:

  -- _ <- newShip $ defShip Player1 (InFlight (0,0) tannen NotBoosted) & shipTypeL .~ Station
  -- _ <- newShip $ defShip Player1 (InFlight (0,0) t NotBoosted) & shipTypeL .~ Monitor

  -- For testing pagination:

  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen) & shipShieldsL .~ False
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen) & shipTypeL .~ Monitor
  _ <- newShip $ defShip Player1 (AtBase tannen) & shipTypeL .~ Station
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen) & shipShieldsL .~ False
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player1 (AtBase tannen)
  _ <- newShip $ defShip Player2 (AtBase tannen) & shipTypeL .~ Monitor
  _ <- newShip $ defShip Player2 (AtBase tannen) & shipTypeL .~ Station
  _ <- newShip $ defShip Player2 (AtBase tannen)

  pure ()
  where
    def = defPlace

defPlace :: Point -> Text -> Place
defPlace point name =
  Place
    { placeName = name
    , placePoint = point
    , placeSize = Medium
    , placeType =
        PBase Base
          { baseOwner = Neutral mempty
          , basePopulation = Outpost
          , baseDisease = Healthy
          , baseInstallations = mempty
          , baseShields = 0
          , baseBuilding = BuildPopulation
          , baseInProgress = mempty
          }
    }

defShip :: Player -> ShipLocation -> Ship
defShip p l =
  Ship p l Corvette True

newPlace :: Place -> State Model PlaceId
newPlace place = do
  id <- PlaceId <$> newId
  modelPlacesL %= HM.insert id place
  pure id
