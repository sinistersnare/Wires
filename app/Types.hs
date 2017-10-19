module Types
  ( Game
  , SenseInput
  , RenderOutput) where

import qualified FRP.Yampa as Yampa (Event)
import qualified SDL (EventPayload)


data Game = Game {clipPos :: Double}
  deriving Show


-- doesnt SenseInput need to give current game state?
-- AKA wtf do signal functions do.
type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (Game, Bool)
