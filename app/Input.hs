module Input
    ( GameInput
    , parseInput
    , mousePos
    , lbp
    , lbpPos
    , lbDown
    , rbp
    , rbpPos
    , rbDown
    , keyPress
    , keyPressed
    , keyReleased
    , keyPressedRepeat
    , quitEvent
    , module SDL.Input.Keyboard.Codes) where

import Data.Maybe

import FRP.Yampa

import Linear (V2(..))
import Linear.Affine (Point(..))

import SDL.Input.Keyboard.Codes
import qualified SDL

import Types (SenseInput)

-- <| Signal Functions |> --

-- | Current mouse position
mousePos :: SF GameInput (Double,Double)
mousePos = arr inpMousePos

-- | Events that indicate left button click
lbp :: SF GameInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF GameInput (Event (Double,Double))
lbpPos = inpMouseLeft ^>> edgeJust

-- | Is left button down
lbDown :: SF GameInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF GameInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF GameInput (Event (Double,Double))
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF GameInput Bool
rbDown = arr (isJust . inpMouseRight)

keyPress :: SF GameInput (Event SDL.Scancode)
keyPress = inpKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF GameInput (Event ())
keyPressed code =
  keyPress >>^ filterE (code ==) >>^ tagWith ()

keyPressedRepeat :: (SDL.Scancode, Bool) -> SF GameInput (Event ())
keyPressedRepeat (code, rep) =
  keyPressRepeat >>^ filterE (rep ==) >>^ tagWith ()

keyRelease :: SF GameInput (Event SDL.Scancode)
keyRelease = inpKeyReleased ^>> edgeJust

keyReleased :: SDL.Scancode -> SF GameInput (Event ())
keyReleased code =
  keyRelease >>^ filterE (code ==) >>^ tagWith ()

keyPressRepeat :: SF GameInput (Event Bool)
keyPressRepeat = inpKeyRepeat ^>> (edge >>^ tagWith True)

quitEvent :: SF GameInput (Event ())
quitEvent = arr inpQuit >>> edge

-- | Exported as abstract type. Fields are accessed with signal functions.
data GameInput = GameInput
    { inpMousePos    :: (Double, Double)        -- ^ Current mouse position
    , inpMouseLeft   :: Maybe (Double, Double)  -- ^ Left button currently down
    , inpMouseRight  :: Maybe (Double, Double)  -- ^ Right button currently down
    , inpKeyPressed  :: Maybe SDL.Scancode
    , inpKeyReleased :: Maybe SDL.Scancode
    , inpKeyRepeat   :: Bool
    , inpQuit        :: Bool                    -- ^ SDL's QuitEvent
    } deriving (Show)

initGameInput :: GameInput
initGameInput = GameInput { inpMousePos    = (0, 0)
                        , inpMouseLeft   = Nothing
                        , inpMouseRight  = Nothing
                        , inpKeyPressed  = Nothing
                        , inpKeyReleased = Nothing
                        , inpKeyRepeat   = False
                        , inpQuit        = False
                        }


-- | Filter and transform SDL events into events which are relevant to our
--   application
parseInput :: SF SenseInput GameInput
parseInput = accumHoldBy nextGameInput initGameInput

-- | Compute next input
nextGameInput :: GameInput -> SDL.EventPayload -> GameInput
nextGameInput inp SDL.QuitEvent = inp { inpQuit = True }
nextGameInput inp (SDL.MouseMotionEvent ev) =
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
    where P (V2 x y) = SDL.mouseMotionEventPos ev
nextGameInput inp (SDL.KeyboardEvent ev)
    -- Quit key pressed (Q or ESC)
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed &&
      ( SDL.keysymScancode (SDL.keyboardEventKeysym ev) == SDL.ScancodeQ ||
        SDL.keysymScancode (SDL.keyboardEventKeysym ev) == SDL.ScancodeEscape )
      = inp { inpQuit = True }

    | SDL.keyboardEventKeyMotion ev == SDL.Pressed &&
      SDL.keyboardEventRepeat    ev == True
      = inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
            , inpKeyRepeat  = True
            , inpKeyReleased= Nothing }
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed
      = inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
            , inpKeyReleased= Nothing }
    | SDL.keyboardEventKeyMotion ev == SDL.Released
      = inp { inpKeyPressed = Nothing
            , inpKeyRepeat  = False
            , inpKeyReleased= Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
nextGameInput inp (SDL.MouseButtonEvent ev) = inp { inpMouseLeft  = lmb
                                                 , inpMouseRight = rmb }
    where motion = SDL.mouseButtonEventMotion ev
          button = SDL.mouseButtonEventButton ev
          pos    = inpMousePos inp
          inpMod = case (motion,button) of
              (SDL.Released, SDL.ButtonLeft)  -> first (const Nothing)
              (SDL.Pressed, SDL.ButtonLeft)   -> first (const (Just pos))
              (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
              (SDL.Pressed, SDL.ButtonRight)  -> second (const (Just pos))
              _                                      -> id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextGameInput inp _ = inp
