module Types
  ( GameState(..)
  , SenseInput
  , RenderOutput
  , initialGame
  , Shape(..)) where


import qualified FRP.Yampa as Yampa (Event)
import qualified SDL (EventPayload)
import SDL.Vect (Point(..))
import Linear (V2(..))

data Shape =
  Circle (Double, Double) Double -- Position, Radius -- Currently drawn as a Rectangle
  deriving (Show, Eq)

-- TODO: possible 'ugrade?' where you can have a 3rd wire? Would want `playerWires :: [V2]` then
data Player = Player
  {
    playerPos :: Point V2 Double,
    playerWire1 :: Maybe (V2 Double),
    playerWire2 :: Maybe (V2 Double)
  }
  deriving (Show)

data GameState = GameState
  {
    statePlayer :: Player,
    stateLevel :: [Shape],
    stateQuit :: Bool,
    stateShapes :: [Shape]
  } deriving (Show)

initialGame :: GameState
initialGame = GameState { stateShapes = [], stateQuit = False , statePlayer = initialPlayer, stateLevel = [] }

initialPlayer :: Player
initialPlayer = Player { playerPos = P (V2 0 0) , playerWire1 = Nothing , playerWire2 = Nothing}

type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (GameState, Bool)
