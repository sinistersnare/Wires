{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main (main) where

import Control.Concurrent (newMVar, swapMVar)
import Control.Monad
import Data.Text (Text)
import Foreign.C.Types
import FRP.Yampa as Yampa
import SDL
-- import SDL.Input.Keyboard.Codes
import Linear (V4(..), V2(..))

import Input

-- import Types (Game, SenseInput, RenderOutput) -- TODO

data Game = Game {clipPos :: Double}
  deriving Show

-- doesnt SenseInput need to give current game state?
-- AKA wtf do signal functions do.
type SenseInput = Yampa.Event SDL.EventPayload

type RenderOutput = (Game, Bool)


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
  SDL.initializeAll -- SDL.initializeAll ??
  SDL.HintRenderScaleQuality $= SDL.ScaleNearest -- hmmmm wat

  window <- SDL.createWindow
            title
            SDL.defaultWindow { SDL.windowInitialSize = V2 sizeX sizeY,
                                  SDL.windowOpenGL = Just SDL.defaultOpenGL }
  SDL.showWindow window
  -- TODO, why parens? What does this do. Is the _ <- necessary?
  -- Maybe just void $ SDL.glCreateContext window
  -- to throwaway the value.
  --- FIXME dont we need to destroy the context at the end of use?
  _ <- SDL.glCreateContext(window)
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

  -- learning haskell: could this be 2 lines?
  -- t <- SDL.time
  -- lastInteraction <- newMVar t
  -- I think =<< just makes this more concise.
  lastInteraction <- newMVar =<< SDL.time

  -- let senseInput _ = do
  --   polledEvent <- SDL.pollEvent -- pollEvent or pollEvents????
  --   -- This will wrap the Event in a Just (return does that i think)
  --   -- Event is a Yampa event, and youre giving it an SDL.EventPayload
  --   -- Event is used becuase Yampas signal functions take Event.
  --   -- (see Input.hs)
  --   currentTime <- SDL.time
  --   dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
  --   putStrLn $ "dt:: " ++ (show dt)
  --   return (dt, Yampa.Event . SDL.eventPayload <$> polledEvent)

  -- -- TODO: state should just be a collection of objects to render?
  -- --  Maybe `state` type should be
  -- --    (collection-to-render, bool-should-exit)
  -- let renderOutput _ (state, shouldExit) = do
  --   rendererDrawColor renderer $= V4 0 0 255 255
  --   clear renderer
  --   present renderer

  reactimate
    (return NoEvent)
    (\_ -> do
      polledEvent <- SDL.pollEvent
      currentTime <- SDL.time
      dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
      return (dt, Yampa.Event . SDL.eventPayload <$> polledEvent))
    (\_ (state, shouldExit) -> do
      SDL.rendererDrawColor renderer $= V4 0 0 255 255
      SDL.clear renderer
      SDL.present renderer
      return shouldExit)
    sf

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
    cont x = stateTriggered (x) -- TODO: necessary parens?

stateTriggered :: Double -> SF AppInput Double
stateTriggered k0 = switch sf cont
  where
    sf = proc input -> do
      timer <- (k0+) ^<< integral <<< constant 0.1 -< ()
      zoomIn <- release -< input
      returnA -< (timer, zoomIn `tag` timer) :: (Double, Yampa.Event Double)
    cont x = stateReleased (x) -- TODO: necessary parens?

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
    returnA -< qTap

------ < Main > ------

initClip :: Double
initClip = 0

gameSession :: SF AppInput Game
gameSession = proc input -> do
    timer <- stateReleased initClip -< input
    returnA -< Game timer

game :: SF AppInput Game
game = switch sf (\_ -> game)
    where sf = proc input -> do
                    gameState <- gameSession -< input
                    gameOver <- exitTrigger -< input
                    returnA -< (gameState, gameOver)

render :: Game -> Game
-- render (Game clip) = Game
render = id -- TODO

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

main :: IO ()
main = animate
        "Wires :("
        (800, 600)
        (parseWinInput >>> ((game >>^ render) &&& handleExit))
      -- I think this is the type...
      -- (SF (Event SDL.EventPayload) (Clip, Bool))
      -- Clip is the game state
      -- Bool is shouldExit
