{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main (main) where

import Control.Concurrent (newMVar, swapMVar)
import Control.Monad
import Data.Text (Text)
import Foreign.C.Types -- (CInt) -- TODO, why does adding the import make it fail?
import FRP.Yampa as Yampa
import SDL
import SDL.Vect (Point(..))
import Linear (V4(..), V2(..))

import Input

import Types (GameState(..), SenseInput, RenderOutput, initialGame, Shape(..)) -- TODO


{-
  ## Main Game Code ##
  - Need to flesh out senseInput
  - Need to flesh out everything
  - How should game state look?
  -- Give update a deltaTime instead of a fixed timeStep????
    https://wiki.haskell.org/Yampa/reactimate check the Example section for howto
  -- hitting EXIT button goes thru my game and makes it wait a few seconds before exiting
      Should use a different system between my 'quit' and real quit.
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

------ < Animation > ------

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

sense :: Bool -> IO (DTime, Maybe (Yampa.Event EventPayload))
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
  forM_ (stateShapes state) (\(Circle (x, y) rad) ->
    SDL.fillRect renderer $
      Just (SDL.Rectangle
              (P (V2 (CInt $ round x) (CInt $ round y)))
              (V2 (CInt $ round rad) (CInt $ round rad))))
  SDL.present renderer
  return shouldExit

animate :: Text -> (CInt, CInt) -> SF SenseInput RenderOutput -> IO ()
animate title (width, height) sf = do
  window <- openWindow title (width, height)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  reactimate (return NoEvent) sense (actuate renderer) sf

  closeGame window renderer

-- This is the amount of time that the game should take to quit
-- after `quitTime` seconds, the game is free to quit.
quitTime :: Double
quitTime = 3

quitGame :: Yampa.Time -> GameState -> SF GameState Bool
quitGame qt stateAtQuit = switch (Yampa.constant False &&& after qt ()) (\_ -> Yampa.constant True)

-- While we are waiting to quit, tell it not to quit. (False)
-- After the quitTime seconds are done, then quit (return True).
-- This seems like duplicating functionality, 2 different 'quits'
-- But I think the idea is that there is a 'quit' as in 'lost'
--   and there is a 'quit' as in what to do after the game is finished (restart or end simulation).
handleExit :: SF GameState Bool
handleExit = switch (Yampa.constant False &&& quitOrLost) (quitGame quitTime)

gameOver :: GameState -> SF GameInput GameState
gameOver stateAtQuit = proc input -> do
  let circ = Circle (250, 250) 100
  let state = stateAtQuit { stateShapes = (circ:(stateShapes stateAtQuit)) } -- FIXME: ugly
  returnA -< state

-- TODO: figure out types for first arg of switch... why is identity needed?
wholeGame :: SF GameInput GameState
wholeGame = switch ((runGame initialGame) >>> (Yampa.identity &&& quitOrLost)) gameOver

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
  let curShapes = stateShapes gameState
  let newCircles = updateShapes $ doEventIf (\pos -> (Circle pos 25):(curShapes)) curShapes mousePos

  let newState = gameState { stateQuit = (isEvent didQuit) , stateShapes = newCircles}
  returnA -< (Yampa.Event newState)


-- updateShapes shapes = map (\(Circle (x, y) rad) -> Circle (x, y+(5 * refreshTime)) rad) shapes
-- Updates all current shapes
updateShapes :: [Shape] -> [Shape]
updateShapes shapes = filterMap updateCircle shapes

updateCircle :: Shape -> Maybe Shape
updateCircle shp@(Circle (x, y) rad) =
  if x > gameWidth || x < 0 || y > gameHeight || y < 0
    then Nothing
    else Just $ Circle (x, y + (5 * refreshTime)) rad

-- look at outOfMoves from 2048/Game.hs
-- Return an event when we want to quit I think.
-- Switch works such that when an event is given, the switch activates.
-- So return NoEvent until (quit) is true. The event is given to the switch.
quitOrLost :: SF GameState (Yampa.Event GameState)
quitOrLost = proc gameState -> do
  let quit = stateQuit gameState
  -- let ev = (Yampa.Event gameState) `gate` (quit)
  returnA -< (Yampa.Event gameState) `gate` (quit)

gameLoop :: SF (Yampa.Event SDL.EventPayload) (GameState, Bool)
gameLoop = parseInput >>> wholeGame >>> (Yampa.identity &&& handleExit)


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


{- DONT UNDERSTAND
  Why `identity` is neede in wholeGame.
  Why `tag` is necessary, just make a new event??
      I guess if its NoEvent itll stay NoEvent, but if its Event itll replace??
 -}


------ < Utility > ------


-- Calls the function on the events value if the event exists
--  if the event does not exist, then the default is taken (the second arg)
--  if the event exists, nothing is done with the second argument.
doEventIf :: (a -> b) -> b -> Yampa.Event a -> b
doEventIf _ def NoEvent = def
doEventIf f _ (Yampa.Event v) = f v

-- From http://snipplr.com/view/59474/simultaneous-filter-and--map/
-- Prepend an element to a list if available.  Leave the list as it is if the
-- first argument is Nothing.
maybecons :: Maybe t -> [t] -> [t]
maybecons Nothing l = l
maybecons (Just e) l = e : l
-- Variant of map which deletes elements if the map function returns Nothing.
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (a:as) = maybecons (f a) $ filterMap f as
