{-# LANGUAGE OverloadedStrings #-}

-- stack ghci --main-is flappy-haskell:exe:flappy-haskell --ghc-options="-lsdl2_image"

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Linear
import           Linear.Affine
import           Prelude            hiding (init)
import           Foreign.C.Types
import qualified SDL
import           SDL (($=))
import qualified SDL.Image

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.Image.load filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> IO ()
renderTexture r (Texture t size) xy =
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy size)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Flappy Haskell" SDL.defaultWindow { SDL.windowInitialSize = V2 300 600 }
  SDL.showWindow window
  -- try loading an asset
  r <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = True
         }
  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r
  texture <- loadTexture r "assets/bird-01.png"
  SDL.clear r
  renderTexture r texture (P (V2 150 300))
  SDL.present r

  threadDelay 20000000

  SDL.destroyRenderer r
  SDL.destroyWindow window
  SDL.quit
