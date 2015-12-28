module Graphics
    (Texture(..),
     loadTexture,
     renderTexture,
     renderTextureRotated,
     renderRepeatedTexture,
     renderRepeatedTextureY) where

import           Control.Monad
import           Foreign.C.Types
import           Linear          hiding (identity)
import           Linear.Affine
import           SDL             (($=))
import qualified SDL
import qualified SDL.Image


data Texture = Texture { getSDLTexture :: SDL.Texture
                       , coord         :: V2 CInt
                       }

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.Image.load filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return $ Texture t size

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> IO ()
renderTexture r (Texture t size) xy =
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy size)

renderTextureRotated :: SDL.Renderer -> Texture -> Point V2 CInt -> CDouble -> IO ()
renderTextureRotated r (Texture t size) xy ang =
  SDL.copyEx r t Nothing (Just $ SDL.Rectangle xy size) ang Nothing (V2 False False)

renderRepeatedTexture :: SDL.Renderer -> Texture -> CInt -> CInt -> IO ()
renderRepeatedTexture r t@(Texture _ (V2 width height)) ox oy = do
  renderTexture r t (P (V2 offset (oy-height)))
  renderTexture r t (P (V2 (offset+width) (oy-height)))
  where
    offset = ox - (ox `div` width) * width - width

renderRepeatedTextureY :: SDL.Renderer -> Texture -> CInt -> [CInt] -> IO ()
renderRepeatedTextureY r t ox xs =
  forM_ xs $ \oy ->
    renderTexture r t (P (V2 ox oy))
