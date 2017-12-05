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

import Debug.Trace

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
  playerVelocity :: V2 Double
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

updateWire :: Double -> [Shape] -> Player -> Wire -> Wire
updateWire addTime level player wire@Wire{wireLiveTime=liveTime, wireSprite=sprite, wireDirection=dir} =
  let (V2 dx dy) = dir in
  let newLiveTime = liveTime + addTime in
  let (SDL.Rectangle pos (V2 w h)) = spriteGetBounds sprite in
  let newSize :: V2 CInt = if isColliding (wireGetBounds wire) level
      then (V2 w h)
      else V2 w $ round $ newLiveTime * 1 * dy + fromIntegral h in
  wire { wireLiveTime = newLiveTime
       , wireSprite = spriteSetBounds sprite $
                        SDL.Rectangle (playerGetPos player)
                                      (fmap fromIntegral newSize)}

-- destroys player
destroyPlayer :: Player -> IO ()
destroyPlayer p = do
  destroySprite $ playerSprite p
  tex <- playerWireTex p
  SDL.destroyTexture tex

initialGame :: SDLData -> GameState
initialGame sdlData =
  GameState { stateQuit = False
            , statePlayer = (initialPlayer (P $ V2 400 100) sdlData)
            , stateLevel = [ (Rect (P (V2 50 400)) (V2 700 50))
                           , (Rect (P (V2 50 0)) (V2 700 50))]
  }

initialPlayer :: Point V2 Double -> SDLData -> Player
initialPlayer pos sdlData =
  let rnd = sdlRenderer sdlData in
  let playerSize = V2 50 100 in -- TODO: dont hardcode player's size.
  Player {
      playerSprite = spriteFromFilePath pos playerSize rnd "./assets/player.png"
    , playerWires = []
    , playerMaxWires = 1
    , playerWireTex = loadTexture rnd "./assets/wire.png"
    , playerVelocity = V2 0.0 0.0
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

isColliding :: SDL.Rectangle CInt -> [Shape] -> Bool
isColliding w [] = False
isColliding w (lb@(Rect (P (V2 sx sy)) (V2 sw sh)):ss) =
  let wsb@(SDL.Rectangle (P (V2 wx wy)) (V2 ww wh)) = w in
  let (ax1, ay1) = (wx, wy) in
  let (ax2, ay2) = (wx+ww, wy+wh) in
  let (bx1, by1) = (round sx, round sy) in
  let (bx2, by2) = (round $ sx+sw, round $ sy+sh) in
  let collided = ((ax1 < bx2) && (ax2 > bx1) && (ay1 < by2) && (ay2 > by1)) in
  -- (trace (show (wsb, lb)) collided) || (isColliding w ss)
  collided || (isColliding w ss)

-- might not draw size correctly, something something pythagoreas
-- get bounds and resize, then angle? or vice-versa?
-- TODO: this function is pretty gross...
drawWire :: SDL.Renderer -> Player -> [Shape] -> Wire -> IO ()
drawWire renderer player level@(l:ls) wire@Wire{wireSprite=ws} =
  let (SDL.Rectangle pos@(P vp@(V2 x y)) size) = spriteGetBounds ws in
  let dir@(V2 dx dy) = wireDirection wire :: V2 Double in
  let newBounds = SDL.Rectangle pos (fmap fromIntegral size) in
  -- let ang = getAngle (fmap fromIntegral vp) dir in
  let ang = 0 in
  let ws' = spriteSetBounds ws (fmap fromIntegral newBounds) in
  let ws'' = spriteSetAngle ws' ang in
  drawSprite  renderer ws''


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
