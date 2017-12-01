{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types
  ( GameState(..)
  , SenseInput
  , RenderOutput
  , initialGame
  , Shape(..)
  , Player(..)
  , playerGetPos
  , drawPlayer
  , drawLevel
  , Wire(..)
  , createWire
  , updateWire
  , isColliding
  , getAngle
  , playerGetBounds
  , wireGetBounds
  , playerSetPos) where

import Foreign.C.Types (CInt(..))
import Control.Monad

import SDLData (SDLData(..), destroySDLData)
import qualified FRP.Yampa as Yampa (Event)
import SDL (EventPayload)
import qualified SDL
import SDL.Vect (Point(..))
import Linear (V2(..), normalize)
import SDL.Image (loadTexture)
import Sprite (spriteFromFilePath, Sprite(..), drawSprite,
                destroySprite, spriteFromTex,
                spriteGetBounds, spriteSetBounds,
                spriteSetAngle, spriteSetPos)

data Shape =
    Circle (Point V2 Double) Double
  | Rect (Point V2 Double) (V2 Double)
  deriving (Show, Eq)

data Wire = Wire {
  wireDirection :: V2 Double, -- ^ normalized Direction vector of wire.
  wireLiveTime :: Double, -- ^ Seconds that wire has been alive.
  wireSprite :: Sprite
} deriving (Show)

data Player = Player {
  playerSprite :: Sprite,
  playerWires :: [Wire],
  playerMaxWires :: Int,
  playerWireTex :: IO SDL.Texture,
  playerVelocityX :: Double,
  playerVelocityY :: Double
}

data GameState = GameState {
  statePlayer :: Player,
  stateLevel :: [Shape],
  stateQuit :: Bool
} deriving (Show)

instance Show Player where
  show player =
    let wires = playerWires player in
    let maxWires = playerMaxWires player in
    let sprite = playerSprite player in
    ("Player<Sprite: " ++ (show sprite)
      ++ ", Wires: " ++ (show wires)
      ++ ", MaxWires: " ++ (show maxWires)
      ++ ">")

createWire :: Player -> V2 Double -> Wire
createWire player dir =
  let pos = playerGetPos player in
  let rnd = playerGetRenderer player in
  Wire { wireDirection = dir
       , wireLiveTime = 0
       , wireSprite = spriteFromTex pos (V2 10 30) rnd (playerWireTex player)
  }

updateWire :: Wire -> Double -> Wire
updateWire wire addTime = wire { wireLiveTime = ((wireLiveTime wire) + addTime) }

-- destroys player
destroyPlayer :: Player -> IO ()
destroyPlayer p = do
  destroySprite $ playerSprite p
  tex <- playerWireTex p
  SDL.destroyTexture tex

initialGame :: SDLData -> GameState
initialGame sdlData =
  GameState { stateQuit = False
            , statePlayer = (initialPlayer (P $ V2 400 300) sdlData)
            , stateLevel = [(Rect (P (V2 50 400)) (V2 700 50))]
  }

initialPlayer :: Point V2 Double -> SDLData -> Player
initialPlayer pos sdlData =
  let rnd = sdlRenderer sdlData in
  let playerSize = V2 50 100 in -- TODO: dont hardcode player's size.
  Player {
      playerSprite = spriteFromFilePath pos playerSize rnd "./assets/player.png"
    , playerWires = []
    , playerMaxWires = 2
    , playerWireTex = loadTexture rnd "./assets/wire.png"
    , playerVelocityX = 0.0
    , playerVelocityY = 0.0015
  }

playerGetPos :: Player -> Point V2 Double
playerGetPos = (spritePos . playerSprite)

playerSetPos :: Player -> Point V2 Double -> Player
playerSetPos player@Player{playerSprite=s} newPos =
  let s' = spriteSetPos s newPos in
  player { playerSprite = s' }

playerGetBounds :: Player -> SDL.Rectangle CInt
playerGetBounds = spriteGetBounds . playerSprite

wireGetBounds :: Wire -> SDL.Rectangle CInt
wireGetBounds = spriteGetBounds . wireSprite

drawPlayer :: SDL.Renderer -> GameState -> IO ()
drawPlayer renderer state = do
  let player = statePlayer state
  let level = stateLevel state
  let sprite = playerSprite player
  drawSprite renderer sprite
  forM_ (playerWires player) (drawWire renderer player level)

isColliding :: Wire -> [Shape] -> Bool
isColliding w [] = False
isColliding w ((Rect (P (V2 x y)) (V2 l h)):ss) =
  let (SDL.Rectangle (P (V2 wx wy)) (V2 ww wh)) = spriteGetBounds (wireSprite w) in
  if wx >= (round x) && wx <= (round $ x + l) && wy >= (round y) && wy <= (round $ y + h)
    then True
    else
      isColliding w ss

-- might not draw size correctly, something something pythagoreas
-- get bounds and resize, then angle? or vice-versa?
-- TODO: this function is pretty gross...
drawWire :: SDL.Renderer -> Player -> [Shape] -> Wire -> IO ()
drawWire renderer player level wire =
  let ws = wireSprite wire in
  let (SDL.Rectangle (P pos@(V2 x y)) (V2 w h)) = spriteGetBounds ws in
  let liveTime = wireLiveTime wire in
  let dir@(V2 dx dy) = wireDirection wire :: V2 Double in
  let newSize :: V2 CInt = if isColliding wire level
      then V2 w h
      else V2 (round $ liveTime * dx + dx) (round $ liveTime * dy + dy) in
  let newBounds = SDL.Rectangle (playerGetPos player) (fmap fromIntegral newSize) in
  let ang = getAngle (fmap fromIntegral pos) dir in -- pos is CInt we need Double...
  let ws' = spriteSetBounds ws newBounds in
  let ws'' = spriteSetAngle ws' ang in
  drawSprite renderer ws''


-- ^ Gets angle between 2 vectors. RETURNS DEGREES
getAngle :: V2 Double -> V2 Double -> Double
getAngle v1 v2 = toDegrees $ acos $ dot n1 n2
  where
    (n1, n2) = (normalize v1, normalize v2)
    toDegrees r = r * 180 / pi
    dot (V2 v1x v1y) (V2 v2x v2y) = (v1x * v2x) + (v1y * v2y)

drawLevel :: SDL.Renderer -> [Shape] -> IO ()
drawLevel renderer level = do
  forM_ level (drawShape renderer)

drawShape :: SDL.Renderer -> Shape -> IO ()
drawShape renderer (Circle pos crad) =
  let rad = CInt $ round crad in
  SDL.fillRect renderer $ Just $ SDL.Rectangle (fmap round pos) (V2 rad rad)
drawShape renderer (Rect pos dims) =
  SDL.fillRect renderer $ Just $ SDL.Rectangle (fmap round pos) (fmap round dims)

type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (GameState, Bool)

playerGetRenderer :: Player -> SDL.Renderer
playerGetRenderer player = spriteRenderer . playerSprite $ player
