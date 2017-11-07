{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main (main) where

import Data.Maybe (mapMaybe)
import Control.Concurrent (newMVar, swapMVar)
import Control.Monad
import Data.Text (Text, isSuffixOf)
import Foreign.C.Types (CInt(..))
import FRP.Yampa as Yampa
import SDL (($=))
import qualified SDL
import SDL.Image (loadTexture)
import SDL.Vect (Point(..))
import Linear (V4(..), V2(..))

import Input

import Types (GameState(..), SenseInput, RenderOutput, initialGame, Shape(..), Player(..)) -- TODO


{-
  ## Main Game Code ##
  - Need to flesh out senseInput
  - Need to flesh out everything
  - How should game state look?
  -- Give update a deltaTime instead of a fixed timeStep????
    https://wiki.haskell.org/Yampa/reactimate check the Example section for howto
  -- hitting EXIT button goes thru my game and makes it wait a few seconds before exiting
      Should use a different system between my 'quit' and real quit.

  -- SDL.Video.Renderer.copyEx to rotate texxxtures
  -- TODO: Sprite class (model after LibGDX Sprite.java)
-}

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizeX, sizeY) = do
  SDL.initializeAll
  SDL.HintRenderScaleQuality $= SDL.ScaleNearest

  window <- SDL.createWindow
            title
            SDL.defaultWindow { SDL.windowInitialSize = V2 sizeX sizeY,
                                  SDL.windowOpenGL = Just SDL.defaultOpenGL }
  SDL.showWindow window
  -- FIXME: dont we need to destroy the created context at the end of use?
  --        SDL doesnt have a destroyContext function sooo
  void $ SDL.glCreateContext window
  return window

closeGame :: SDL.Window -> SDL.Renderer -> IO ()
closeGame window renderer = do
  SDL.destroyWindow window
  SDL.destroyRenderer renderer
  SDL.quit

-- refresh time in milliseconds
refreshTime' :: Integer
refreshTime' = 15

-- refresh time in seconds
-- ## !! This will be the delta time used in physics calculations !! ##
refreshTime :: Double
refreshTime = fromInteger refreshTime' * 10^^(-3)

{-
    ##in animate:
    lastInteraction <- newMVar =<< SDL.time
    ##in sense function:
    polledEvent <- SDL.pollEvent
    currentTime <- SDL.time
    dt <- return refreshTime -- (currentTime -) <$> swapMVar lastInteraction currentTime
    ##
    this gives a delta time, but how do we get that into `update`???
-}

sense :: Bool -> IO (DTime, Maybe (Yampa.Event SDL.EventPayload))
sense _ = do
  polledEvent <- SDL.pollEvent
  return (refreshTime, Yampa.Event . SDL.eventPayload <$> polledEvent)

actuate :: SDL.Renderer -> Bool -> (GameState, Bool) -> IO Bool
actuate renderer _ (state, shouldExit) = do
  let r = 0
      g = 0
      b = 255
  SDL.rendererDrawColor renderer $= V4 r g b 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  renderGame state renderer
  SDL.present renderer
  return shouldExit

renderGame :: GameState -> SDL.Renderer -> IO ()
renderGame state renderer = do
  forM_ (stateLevel state) (\(Circle (P (V2 x y)) rad) ->
    SDL.fillRect renderer $
      Just (SDL.Rectangle
            (P (V2 (CInt $ round x) (CInt $ round y)))
            (V2 (CInt $ round rad) (CInt $ round rad))))
  let P (V2 x y) = (playerPos $ statePlayer $ state)
  SDL.fillRect renderer $
      Just (SDL.Rectangle
            (P (V2 (round x) (round y)))
            (V2 (CInt $ round 50) (CInt $ round 100)))
  when (stateQuit state) $ do
    SDL.rendererDrawColor renderer $= V4 255 0 0 255
    SDL.fillRect renderer $
      Just (SDL.Rectangle
              (P $ V2 (round 0) (round 0))
              (V2 (round 100) (round 100)))

animate :: Text -> (CInt, CInt) -> SF SenseInput RenderOutput -> IO ()
animate title (width, height) sf = do
  window <- openWindow title (width, height)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  reactimate (return NoEvent) sense (actuate renderer) sf
  closeGame window renderer

-- | Run the game, keeping the internal state using dHold, updating the
-- game state based on user's input (if any)
runGame :: GameState -> SF GameInput GameState
runGame state = proc input -> do
  rec currentState <- dHold state -< gameUpdated
      gameUpdated <- update -< (input, currentState)
  returnA -< currentState

update :: SF (GameInput, GameState) (Yampa.Event GameState)
update = proc (input, gameState) -> do
  didQuit <- Input.quitEvent -< input
  mousePos <- Input.lbpPos -< input
  let curShapes = stateLevel gameState
  let newCircles = updateShapes $ doIfEvent
                                    (\(x, y) -> (Circle (P $ V2 x y) 15):(curShapes))
                                    curShapes mousePos

  let newState = gameState { stateQuit = (isEvent didQuit) , stateLevel = newCircles}
  returnA -< (Yampa.Event newState)

-- Updates all current shapes
updateShapes :: [Shape] -> [Shape]
updateShapes shapes = mapMaybe updateCircle shapes

updateCircle :: Shape -> Maybe Shape
updateCircle shp@(Circle (P (V2 x y)) rad) =
  if x > gameWidth || x < 0 || y > gameHeight || y < 0
    then Nothing
    else Just $ Circle (P $ V2 x (y + (10 * refreshTime))) rad

gameLoop :: SF (Yampa.Event SDL.EventPayload) (GameState, Bool)
gameLoop = parseInput >>> wholeGame >>> (Yampa.identity &&& handleExit)

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
handleExit :: SF GameState Bool
handleExit = switch (Yampa.constant False &&& quitOrLost) (quitGame quitTime)

gameOver :: GameState -> SF GameInput GameState
gameOver stateAtQuit = proc input -> do
  -- things to do when gameOver happens.
  returnA -< stateAtQuit

wholeGame :: SF GameInput GameState
wholeGame = switch ((runGame initialGame) >>> (Yampa.identity &&& quitOrLost)) gameOver


------ < Main > ------

main :: IO ()
main = animate
        "Wires :("
        (gameWidth, gameHeight)
        gameLoop

gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

------ < Utility > ------

-- Calls the function on the events value if the event exists
--  if the event does not exist, then the default is taken (the second arg)
--  if the event exists, nothing is done with the second argument.
doIfEvent :: (a -> b) -> b -> Yampa.Event a -> b
doIfEvent _ def NoEvent = def
doIfEvent f _ (Yampa.Event v) = f v
