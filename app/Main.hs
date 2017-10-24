{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main (main) where

import Control.Concurrent (newMVar, swapMVar)
import Control.Monad
import Data.Text (Text)
import Foreign.C.Types -- (CInt) -- TODO, why does adding the import make it fail?
import FRP.Yampa as Yampa
import SDL
import SDL.Vect
-- import SDL.Input.Keyboard.Codes
import Linear (V4(..), V2(..))

import Input

import Types (Game(..), SenseInput, RenderOutput, initialGame, Shape(..)) -- TODO


{-
  ## Main Game Code ##
  - currently 100% not working
  - Need to flesh out senseInput
  - Need to flesh out everything
  - how do Signal Functions work?
  - How should game state look?
  - How should renderOutput argument look (time, ???)
  -   Probably just a list of objects to render?
  -   Or maybe the game state, that we need to turn into
        A list of objects to render?
        Depends on how SFs work I think
  -
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

animate :: Text -> (CInt, CInt) -> SF SenseInput RenderOutput -> IO ()
animate title (width, height) sf = do
  window <- openWindow title (width, height)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  lastInteraction <- newMVar =<< SDL.time

  reactimate
    (return NoEvent)
    (\_ -> do
      polledEvent <- SDL.pollEvent
      currentTime <- SDL.time
      dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
      return (dt, Yampa.Event . SDL.eventPayload <$> polledEvent))
    (\_ (state, shouldExit) -> do
      time <- SDL.time
      let r = 0 -- round $ (((cos time) + 1) / 2) * 255
          g = 0 -- round $ (((sin time) + 1) / 2) * 255
          b = 255 -- round $ (((sin time) + 1) / 2) * 255
      SDL.rendererDrawColor renderer $= V4 r g b 255
      SDL.clear renderer
      SDL.rendererDrawColor renderer $= V4 0 0 0 255
      -- putStrLn ("shapes:: " ++ (show $ stateShapes state))
      forM_ (stateShapes state) (\(Circle (x, y) rad) ->
        SDL.fillRect
          renderer $
          Just
            (SDL.Rectangle
              (P (V2 (CInt $ round x) (CInt $round y)))
              (V2 (CInt $ round rad) (CInt $round rad))))
      -- let (Circle (x, y) rad) = (head $ stateShapes state)
      -- drawFilledCircle renderer (P (V2 (CInt $ round x) (CInt $round y))) (round rad) (V4 0 0 0 255)
      SDL.present renderer
      return shouldExit)
    sf

  -- FIXME: figure out why it wont let me use `let` in this do block.
  -- reactimate
  --   (return NoEvent)
  --   senseInput
  --   renderOutput
  --   sf

  closeGame window renderer

------ < Input Handling > ------

stateReleased :: Double -> SF AppInput Double
stateReleased k0 = switch sf cont
  where
    sf = proc input -> do
      timer <- constant k0 -< ()
      zoomIn <- trigger -< input
      returnA -< (timer, zoomIn `tag` timer) :: (Double, Yampa.Event Double)
    cont = stateTriggered

stateTriggered :: Double -> SF AppInput Double
stateTriggered k0 = switch sf cont
  where
    sf = proc input -> do
      timer <- (k0+) ^<< integral <<< constant 0.1 -< ()
      zoomIn <- release -< input
      returnA -< (timer, zoomIn `tag` timer) :: (Double, Yampa.Event Double)
    cont = stateReleased

trigger :: SF AppInput (Yampa.Event ())
trigger =
  proc input -> do
    -- TODO: can these 2 line be swapped?
    unTapHold <- keyPressedRepeat (SDL.ScancodeSpace, True) -< input
    unTap <- keyPressed (SDL.ScancodeSpace) -< input
    returnA -< lMerge unTap unTapHold -- cause the ordering here is weird

release :: SF AppInput (Yampa.Event ())
release =
  proc input -> do
    unTap <- keyReleased (SDL.ScancodeSpace) -< input
    returnA -< unTap

exitTrigger :: SF AppInput (Yampa.Event ())
exitTrigger =
  proc input -> do
    qTap <- keyPressed ScancodeQ -< input
    escTap <- keyPressed ScancodeEscape -< input
    returnA -< lMerge qTap escTap

------ < Logic > ------

gameSession :: SF AppInput Game
gameSession = proc input -> do
    returnA -< initialGame

game :: SF AppInput Game
game = switch gameNotOver gameOver
    where
      gameNotOver = proc input -> do
        gameState <- gameSession -< input
        isGameOver <- exitTrigger -< input
        returnA -< (gameState, isGameOver)
      -- gameOver called when gameOver ^^ is Event, usually its NoEvent
      gameOver _ = proc input -> do
        let circ = Circle (100, 100) 50
        let state = initialGame { stateShapes = [circ] }
        returnA -< state

render :: Game -> Game
render = id
        -- maybePos <- lbpPos -< input
        -- let shapes = if isEvent maybePos then [Circle (fromEvent maybePos) 25] else (stateShapes gameState)
        -- let newState = gameState {stateShapes = shapes}

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

gameLoop :: SF (Yampa.Event SDL.EventPayload) (Game, Bool)
gameLoop = parseInput >>> ((game >>^ render) &&& handleExit)

------ < Main > ------

main :: IO ()
main = animate
        "Wires :("
        (800, 600)
        gameLoop
