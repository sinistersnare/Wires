module Sprite (Sprite(..), spriteFromFilePath, drawSprite) where

import SDLData (SDLData(..))
import qualified SDL
import SDL.Video.Renderer (Texture)
import Foreign.C.Types (CInt(..))
import SDL.Vect (Point(..))
import SDL.Image (loadTexture)
import Linear (V2(..), V4(..))

-- TODO: have a 'dead' field, so when destroySprite called, sprite -> unusable?

-- TODO: can we make only the texture an IO? might clean up a little of the code.
data Sprite = Sprite {
  spritePos :: Point V2 Double,
  spriteDims :: V2 Double,
  spriteTex :: Texture,
  spriteRenderer :: SDL.Renderer
}

instance Show Sprite where
  show sprite =
    let (P (V2 x y)) = spritePos sprite in
    let (V2 width height) = spriteDims sprite in
    "<Sprite: x: " ++ (show x) ++ ", y: " ++ (show y) ++ ", width: " ++ (show width) ++ ", height: " ++ (show height) ++ ">"

spriteFromFilePath :: Point V2 Double -> V2 Double -> SDL.Renderer -> FilePath -> IO Sprite
spriteFromFilePath pos dimensions rnd file = do
  tex <- loadTexture rnd file
  return $ Sprite { spritePos = pos , spriteTex = tex , spriteRenderer = rnd , spriteDims = dimensions}

getSpriteBounds :: Sprite -> SDL.Rectangle CInt
getSpriteBounds sprite =
  let pos = fmap round $ spritePos sprite in
  let dimensions = fmap round $ spriteDims sprite in
  SDL.Rectangle pos dimensions

drawSprite :: SDL.Renderer -> Sprite -> IO ()
drawSprite renderer sprite = do
  let (P (V2 x y)) = spritePos sprite
  let texBounds = getSpriteBounds sprite
  SDL.copy renderer (spriteTex sprite) Nothing (Just texBounds) -- Nothing $ Just $ SDL.Rectangle (P (V2 400 300)) (V2 100 50)

destroySprite :: IO Sprite -> IO ()
destroySprite s = do
  sprite <- s
  let tex = spriteTex sprite
  SDL.destroyTexture tex
