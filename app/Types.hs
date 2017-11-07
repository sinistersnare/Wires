module Types
  ( GameState(..)
  , SenseInput
  , RenderOutput
  , initialGame
  , Shape(..)
  , Player(..)
  , getPlayerPos
  , drawPlayer) where

import Sprite (spriteFromFilePath, Sprite(..), drawSprite)
import SDLData (SDLData(..), destroySDLData)
import qualified FRP.Yampa as Yampa (Event)
import SDL (EventPayload)
import qualified SDL
import SDL.Vect (Point(..))
import Linear (V2(..))

data Shape =
  Circle (Point V2 Double) Double
  deriving (Show, Eq)

-- TODO: possible 'ugrade?' where you can have a 3rd wire? Would want `playerWires :: [V2]` then
data Player = Player {
    playerSprite :: IO Sprite,
    playerWire1 :: Maybe (V2 Int),
    playerWire2 :: Maybe (V2 Int)
  } -- deriving (Show) -- TODO: make Show (cant becuase Sprite)

data GameState = GameState {
  statePlayer :: Player,
  stateLevel :: [Shape],
  stateQuit :: Bool
} -- deriving (Show) -- TODO: make Show (cant because Player)

initialGame :: SDLData -> GameState
initialGame sdlData = GameState { stateQuit = False
                            , statePlayer = (initialPlayer (P $ V2 400 300) sdlData)
                            , stateLevel = [] }

initialPlayer :: Point V2 Double -> SDLData -> Player
initialPlayer pos sdlData = Player {
        playerSprite = spriteFromFilePath pos (V2 50 100) (sdlRenderer sdlData) "./assets/player.png"
      , playerWire1 = Nothing
      , playerWire2 = Nothing }

getPlayerPos :: Player -> IO (Point V2 Double)
getPlayerPos player = do
  sprite <- playerSprite player
  return $ spritePos sprite

drawPlayer :: SDL.Renderer -> Player -> IO ()
drawPlayer renderer player = do
  pl <- playerSprite player
  drawSprite renderer pl
  -- TODO: draw wires!?

type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (GameState, Bool)
