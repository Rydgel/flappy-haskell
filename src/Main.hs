{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Linear             hiding (identity)
import           Linear.Affine
import           Prelude            hiding (init)
import           Foreign.C.Types
import qualified SDL
import           SDL (($=))
import qualified SDL.Image
import           FRP.Yampa

data Texture = Texture SDL.Texture (V2 CInt)

getSDLTexture :: Texture -> SDL.Texture
getSDLTexture (Texture t _) = t

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

main' :: IO ()
main' = do
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
  renderTexture r texture (P (V2 0 0))
  SDL.present r

  threadDelay 20000000

  SDL.destroyTexture $ getSDLTexture texture
  SDL.destroyRenderer r
  SDL.destroyWindow window
  SDL.quit

type Pos = Double
type Vel = Double

data Bird = Bird { birdPos :: Double
                 , birdVel :: Double
                 }

fallingBird :: Bird -> SF () Bird
fallingBird (Bird y0 v0) = proc _ -> do
  v <- imIntegral v0 -< -9.81
  y <- imIntegral y0 -< v
  returnA -< Bird y v

main :: IO ()
main = reactimate (return ())
                  (\_ -> threadDelay 100000 >> return (0.1, Nothing))
                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> return False)
                  (fallingBird 10.0 0.0)
