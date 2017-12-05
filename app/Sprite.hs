module Sprite (Sprite(..), spriteFromFilePath,
              drawSprite, spriteFromTex,
              spriteSetBounds, spriteGetBounds, destroySprite,
              spriteSetAngle, spriteSetPos) where

import SDLData (SDLData(..))
import qualified SDL
import SDL.Video.Renderer (Texture)
import Foreign.C.Types (CInt(..), CDouble(..))
import SDL.Vect (Point(..))
import SDL.Image (loadTexture)
import Linear (V2(..), V4(..))

-- TODO: have a 'dead' field, so when destroySprite called, sprite -> unusable?
-- Seems like that would be a correctness issue, and IDK how we would solve it
-- aside from making all methods return a Maybe Sprite, and that would suck.
-- (well the solution is Monads of course, but that may be ugly...)

data Sprite = Sprite {
  spritePos :: Point V2 Double,
  spriteDims :: V2 Double,
  spriteTex :: IO Texture,
  spriteRenderer :: SDL.Renderer,
  spriteAngle :: Double
}

instance Show Sprite where
  show sprite =
    let (P (V2 x y)) = spritePos sprite in
    let (V2 width height) = spriteDims sprite in
    let angle = spriteAngle sprite in
    ("Sprite<x: " ++ (show x)
      ++ ", y: " ++ (show y)
      ++ ", width: " ++ (show width)
      ++ ", height: " ++ (show height)
      ++ ", angle: " ++ (show angle) ++ ">")

spriteFromTex :: Point V2 Double -> V2 Double -> SDL.Renderer -> IO SDL.Texture -> Sprite
spriteFromTex pos dims rnd tex =
  Sprite { spritePos = pos
         , spriteTex = tex
         , spriteRenderer = rnd
         , spriteDims = dims
         , spriteAngle = 0 }

spriteFromFilePath :: Point V2 Double -> V2 Double -> SDL.Renderer -> FilePath -> Sprite
spriteFromFilePath pos dimensions rnd file =
  let tex = loadTexture rnd file in
  spriteFromTex pos dimensions rnd tex

spriteGetBounds :: Sprite -> SDL.Rectangle CInt
spriteGetBounds sprite =
  let pos = fmap round $ spritePos sprite in
  let dimensions = fmap round $ spriteDims sprite in
  SDL.Rectangle pos dimensions

spriteSetBounds :: Sprite -> SDL.Rectangle Double -> Sprite
spriteSetBounds sprite (SDL.Rectangle pos dims) =
  sprite { spritePos = pos , spriteDims = dims }

spriteSetPos :: Sprite -> Point V2 Double -> Sprite
spriteSetPos sprite pos = sprite { spritePos = pos }

spriteSetAngle :: Sprite -> Double -> Sprite
spriteSetAngle sprite ang = sprite { spriteAngle = ang }

drawSprite :: SDL.Renderer -> Sprite -> IO ()
drawSprite renderer sprite = do
  let (P (V2 x y)) = spritePos sprite
  let texBounds@(SDL.Rectangle (P (V2 x y)) (V2 width height)) = fmap fromIntegral $ spriteGetBounds sprite
  tex <- spriteTex sprite
  SDL.copyEx renderer tex Nothing (Just $ fmap round texBounds)
             (CDouble $ spriteAngle sprite)
             (Just $ fmap round $ P (V2 (x + (width / 2)) y {- (y + (height / 2)) -}))
             -- Nothing -- Center can be nothing
             (V2 False False)

destroySprite :: Sprite -> IO ()
destroySprite sprite = do
  tex <- spriteTex sprite
  SDL.destroyTexture tex
