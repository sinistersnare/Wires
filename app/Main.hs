{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main (main) where

import Data.Maybe (mapMaybe)
import Control.Concurrent (newMVar, swapMVar)
import Control.Monad
import Data.Text (Text, isSuffixOf)
import Foreign.C.Types (CInt(..))
import SDL (($=))
import qualified SDL
import SDL.Image (loadTexture)
import SDL.Vect (Point(..))
import qualified SDL.Video.OpenGL as GL
import Linear (V4(..), V2(..))
import FRP.Yampa (lMerge, SF, reactimate
                , dHold, returnA, isEvent
                , (>>>), (&&&), switch, after
                , gate, fromEvent)
import qualified FRP.Yampa as Yampa

import Input (GameInput, parseInput)
import qualified Input
import SDLData (SDLData(..), destroySDLData)
import Types (GameState(..),
              SenseInput, RenderOutput,
              initialGame, Shape(..), Player(..),
              playerGetPos, drawPlayer, drawLevel,
              Wire(..), createWire, updateWire)

import Debug.Trace

{-
  ## Main Game Code ##
  TODO:
  -- Give update a deltaTime instead of a fixed timeStep????
    https://wiki.haskell.org/Yampa/reactimate#Example check the Example section for howto
  -- hitting EXIT button goes thru my game and makes it wait a few seconds before exiting
      Should use a different system between my 'quit' and real quit.
  -- TODO: Sprite class (model after LibGDX Sprite.java)
-}

createSDLData :: Text -> (CInt, CInt) -> IO SDLData
createSDLData title (sizeX, sizeY) = do
  (window, ctx) <- openWindow title (sizeX, sizeY)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return SDLData { sdlRenderer = renderer , sdlWindow = window , sdlContext = ctx}

openWindow :: Text -> (CInt, CInt) -> IO (SDL.Window, GL.GLContext)
openWindow title (sizeX, sizeY) = do
  SDL.initializeAll
  SDL.HintRenderScaleQuality $= SDL.ScaleNearest

  window <- SDL.createWindow title
            SDL.defaultWindow { SDL.windowInitialSize = V2 sizeX sizeY
                              , SDL.windowOpenGL = Just SDL.defaultOpenGL
                              , SDL.windowResizable = True}
  SDL.showWindow window
  ctx <- SDL.glCreateContext window
  return (window, ctx)

closeGame :: SDLData -> IO ()
closeGame sdlData = do
  destroySDLData sdlData
  SDL.quit

-- refresh time in milliseconds
refreshTime' :: Integer
refreshTime' = 15

-- refresh time in seconds
-- ## !! This will be the delta time used in physics calculations !! ##
refreshTime :: Double
refreshTime = fromInteger refreshTime' * 10^^(-3)

sense :: Bool -> IO (Yampa.DTime, Maybe SenseInput)
sense _ = do
  polledEvent <- SDL.pollEvent
  return (refreshTime, Yampa.Event . SDL.eventPayload <$> polledEvent)

actuate :: SDL.Renderer -> Bool -> (GameState, Bool) -> IO Bool
actuate renderer _ (state, shouldExit) = do
  let (r, g, b) = (75,0,130)
  SDL.rendererDrawColor renderer $= V4 r g b 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  renderGame state renderer
  SDL.present renderer
  return shouldExit

renderGame :: GameState -> SDL.Renderer -> IO ()
renderGame state renderer = do
  drawLevel renderer $ stateLevel state
  drawPlayer renderer $ statePlayer state
  when (stateQuit state) $ do
    SDL.rendererDrawColor renderer $= V4 255 0 0 255
    SDL.fillRect renderer $
      Just (SDL.Rectangle
              (P $ V2 (round 0) (round 0))
              (V2 (round 100) (round 100)))

animate :: SDLData -> SF SenseInput RenderOutput -> IO ()
animate sdlData sf = do
  let renderer = sdlRenderer sdlData
  reactimate (return Yampa.NoEvent) sense (actuate renderer) sf
  closeGame sdlData

-- Run the game, keeping the internal state using dHold, updating the
--   game state based on user's input (if any)
runGame :: GameState -> SF GameInput GameState
runGame state = proc input -> do
  rec currentState <- dHold state -< gameUpdated
      gameUpdated <- update -< (input, currentState)
  returnA -< currentState

update :: SF (GameInput, GameState) (Yampa.Event GameState)
update = proc (input, gameState) -> do
  didQuit <- Input.quitEvent -< input
  mousePos <- Input.lbpPos -< input

  let player = statePlayer gameState
  let added = fmap (addWire player) mousePos
  let wires = playerWires player
  let updated = updatePlayerPos (updatePlayerVel (updatePlayer $ fromEvent $ (added `lMerge` (Yampa.Event player))) (isColliding wires))
  let newState = gameState { stateQuit = (isEvent didQuit) , statePlayer = updated }

  returnA -< (Yampa.Event newState)

-- Updates the player velocity
updatePlayerVel :: Player -> Player
updatePlayerVel player False = player { playerYVelocity = playerYVelocity + 9.8}
updatePlayerVel Player((spriteFromFilePath pos ps rnd pth) (w:ws) mwires wiretex xvel yvel) True = 
  let (SDL.Rectangle (P (V2 x y)) (V2 w h)) = spriteGetBounds (wireSprite w) in
  let colPoint = P $ V2 (x + w) (y + h) in 
  player { playerYVelocity = playerYVelocity + 9.8 * sin (getAngle colPoint pos), playerXVelocity = playerXVelocity + 9.8 * cos (getAngle colPoint pos)}

-- Updates the player position
updatePlayerPos :: Player -> Player
updatePlayer Player((spriteFromFilePath (P (V2 x y)) ps rnd pth) wires mwires wiretex xvel yvel) = Player((spriteFromFilePath (P $ V2 (x + xvel) (y + yvel)) ps rnd pth) wires mwires wiretex xvel yvel)

updatePlayer :: Player -> Player
updatePlayer player =
  let newWires = map (flip updateWire refreshTime) (playerWires player) in
  player { playerWires = newWires }

-- Check if a wire is colliding. Can more than one wire collide? Only the first should?
isColliding :: [Wire] -> Bool
isColliding [] = False
isColliding (w:ws) = 
  let (SDL.Rectangle (P (V2 x y)) (V2 w h)) = spriteGetBounds (wireSprite w) in
  if y - h < 100
    then True 
    else
      isColliding ((spriteFromFilePath pos ps rnd pth) ws mwires wiretex xvel yvel))

addWire :: Player -> (Double, Double) -> Player
addWire player (wx, wy) =
  let wires = trace ("updating:: " ++ (show player)) $ playerWires player in
  let (P (V2 px py)) = playerGetPos player in
  let dir = normalize $ V2 (wx - px) (wy - py) in -- not actually good, should shoot out from hands, not playerPos.
  let wire = createWire player dir in
  if (length wires) < (playerMaxWires player)
    then player { playerWires = (wire:wires) }
    else
      player { playerWires = (wire:(sinit wires)) }


gameLoop :: SDLData -> SF (Yampa.Event SDL.EventPayload) (GameState, Bool)
gameLoop sdlData = parseInput >>> (wholeGame sdlData) >>> (Yampa.identity &&& handleExit)

-- This is the amount of time that the game should take to quit
-- after `quitTime` seconds, the game is free to quit.
quitTime :: Double
quitTime = 3

quitGame :: Yampa.Time -> GameState -> SF GameState Bool
quitGame qt stateAtQuit = switch (Yampa.constant False &&& after qt ()) (\_ -> Yampa.constant True)

-- look at outOfMoves from 2048/Game.hs
-- Return an event when we want to quit I think.
-- Switch works such that when an event is given, the switch activates.
-- So return NoEvent until (quit) is true. The event is given to the switch.
quitOrLost :: SF GameState (Yampa.Event GameState)
quitOrLost = proc gameState -> do
  let quit = stateQuit gameState
  returnA -< (Yampa.Event gameState) `gate` (quit)

-- While we are waiting to quit, tell it not to quit. (False for 'dont quit')
-- After the quitTime seconds are done, then quit (return True for 'do quit').
-- This seems like duplicating functionality, 2 different 'quits'
-- But I think the idea is that there is a 'quit' as in 'lost'
--   and there is a 'quit' as in what to do after the game is finished (restart or end simulation).
-- this one is quit as in finished (restart or end simulation)
handleExit :: SF GameState Bool
handleExit = switch (Yampa.constant False &&& quitOrLost) (quitGame quitTime)

-- this one is quit as in 'lost'
gameOver :: GameState -> SF GameInput GameState
gameOver stateAtQuit = proc input -> do
  -- things to do when gameOver happens.
  returnA -< stateAtQuit

wholeGame :: SDLData -> SF GameInput GameState
wholeGame sdlData =
  switch ((runGame $ initialGame sdlData)
           >>> (Yampa.identity &&& quitOrLost))
    gameOver

------ < Main > ------

main :: IO ()
main = do
  sdlData <- createSDLData gameTitle (gameWidth, gameHeight)
  animate sdlData $ gameLoop sdlData

gameTitle :: Text
gameTitle = "Wires! ... :("

gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600



------ < Utility > ------

-- safe init, or sin-init
-- returns [] on an empty list
-- (re-implementation of Haskell's init)
sinit :: [a] -> [a]
sinit [] =  []
sinit (x:[]) = []
sinit (x:xs) = x:(sinit xs)

normalize :: V2 Double -> V2 Double
normalize (V2 x y) = V2 (x / mag) (y / mag)
  where
    mag = sqrt $ (x^^2) + (y^^2)
