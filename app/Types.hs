module Types
  ( Game(..)
  , SenseInput
  , RenderOutput
  , initialGame
  , Shape(..)) where

import qualified FRP.Yampa as Yampa (Event)
import qualified SDL (EventPayload)

-- TODO: rename Game to GameState
-- TODO: rename AppInput to GameInput

data Shape =
  Circle (Double, Double) Double -- Position, Radius
  deriving (Show, Eq)

data Game = Game
  {
    stateShapes :: [Shape],
    stateQuit :: Bool
  }
  deriving (Show)

initialGame :: Game
initialGame = Game { stateShapes = [], stateQuit = False }

-- doesnt SenseInput need to give current game state?
-- AKA wtf do signal functions do.
type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (Game, Bool)
