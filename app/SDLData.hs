module SDLData (SDLData(..), destroySDLData) where


import qualified SDL.Video.OpenGL as GL
import qualified SDL

data SDLData = SDLData {
  sdlWindow :: SDL.Window,
  sdlRenderer :: SDL.Renderer,
  sdlContext :: GL.GLContext
}


destroySDLData :: SDLData -> IO ()
destroySDLData sdlData = do
  SDL.destroyWindow $ sdlWindow sdlData
  SDL.destroyRenderer $ sdlRenderer sdlData
  GL.glDeleteContext $ sdlContext sdlData
