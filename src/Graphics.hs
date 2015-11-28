{-# LANGUAGE OverloadedStrings #-}

module Graphics (animate) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Text           (Text)
import           Data.Word
import           Foreign.C.Types
import           FRP.Yampa
import           Linear              hiding (identity)
import           Linear.Affine
import           Prelude             hiding (init)
import           SDL                 (($=))
import qualified SDL
import qualified SDL.Image

import           Types


data Texture = Texture SDL.Texture (V2 CInt)

data Textures = Textures { bird1T    :: Texture
                         , bird2T    :: Texture
                         , bird3T    :: Texture
                         , bird4T    :: Texture
                         , landT     :: Texture
                         , pipeDownT :: Texture
                         , pipeUpT   :: Texture
                         , pipe      :: Texture
                         , skyT      :: Texture
                         }

backgroundColor :: V4 Word8
backgroundColor = V4 55 201 215 maxBound

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

loadTextures :: SDL.Renderer -> IO Textures
loadTextures r = Textures
             <$> loadTexture r "assets/bird-01.png"
             <*> loadTexture r "assets/bird-02.png"
             <*> loadTexture r "assets/bird-03.png"
             <*> loadTexture r "assets/bird-04.png"
             <*> loadTexture r "assets/land.png"
             <*> loadTexture r "assets/pipe-down.png"
             <*> loadTexture r "assets/pipe-up.png"
             <*> loadTexture r "assets/pipe.png"
             <*> loadTexture r "assets/sky.png"

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

birdSpriteFromState :: Int -> Textures -> Texture
birdSpriteFromState n t = case n `mod` 4 of
  0 -> bird1T t
  1 -> bird2T t
  2 -> bird3T t
  3 -> bird4T t
  _ -> bird1T t

birdAngleFromVelocity :: Double -> CDouble
birdAngleFromVelocity v = realToFrac $ checkMaxRot$ v / 3
  where
    checkMaxRot v' | v' > 90.0  = 90.0
                   | v' < -45.0 = -45.0
    checkMaxRot v'              = v'

renderBird :: SDL.Renderer -> Textures -> Bird -> IO ()
renderBird r t b = renderTextureRotated r birdSprite coord angleBird
  where
    center     = 138 - 34 `div` 2
    coord      = P (V2 center posBird)
    posBird    = round $ birdPos b
    stateBird  = round $ birdState b :: Int
    angleBird  = birdAngleFromVelocity $ birdVel b
    birdSprite = birdSpriteFromState (stateBird `mod` 4) t

renderGame :: SDL.Renderer -> Textures -> CInt -> Game -> IO ()
renderGame r t winHeight g = do
  -- print g
  -- moving sky
  renderRepeatedTexture r (skyT t) posSky (winHeight-112)
  -- FIXME render pipes here
  -- renderTexture r (pipeDownT t) (P (V2 100 0))
  -- renderTexture r (pipeUpT t) (P (V2 100 (winHeight-112-160)))
  -- Moving ground
  renderRepeatedTexture r (landT t) posGround winHeight
  -- The animated bird
  renderBird r t (bird g)
  where
    posGround = round $ groundPos $ ground g
    posSky = round $ skyPos $ sky g

destroyTextures :: Textures -> IO ()
destroyTextures ts = do
  SDL.destroyTexture $ getSDLTexture $ bird1T ts
  SDL.destroyTexture $ getSDLTexture $ bird2T ts
  SDL.destroyTexture $ getSDLTexture $ bird3T ts
  SDL.destroyTexture $ getSDLTexture $ bird4T ts
  SDL.destroyTexture $ getSDLTexture $ landT ts
  SDL.destroyTexture $ getSDLTexture $ pipeDownT ts
  SDL.destroyTexture $ getSDLTexture $ pipeUpT ts
  SDL.destroyTexture $ getSDLTexture $ pipe ts
  SDL.destroyTexture $ getSDLTexture $ skyT ts

animate :: Text                  -- ^ window title
        -> Int                   -- ^ window width in pixels
        -> Int                   -- ^ window height in pixels
        -> SF WinInput WinOutput -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleBest
    window <- SDL.createWindow title windowConf
    SDL.showWindow window
    renderer <- SDL.createRenderer window (-1) renderConf
    SDL.rendererDrawColor renderer $= backgroundColor
    textures <- loadTextures renderer

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
          currentTime <- SDL.time
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          mEvent <- SDL.pollEvent
          return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (obj, shouldExit) = do
          when changed $ do
              SDL.clear renderer
              renderGame renderer textures (fromIntegral winHeight) obj
              SDL.present renderer
          return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    destroyTextures textures
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where
      windowConf = SDL.defaultWindow
         { SDL.windowInitialSize =
             V2 (fromIntegral winWidth) (fromIntegral winHeight)
         , SDL.windowOpenGL = Just SDL.defaultOpenGL
         }
      renderConf = SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = False
         }
