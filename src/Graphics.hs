{-# LANGUAGE OverloadedStrings #-}

module Graphics (animate) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Text           (Text)
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
                         , skyT      :: Texture
                         }

renderGame :: SDL.Renderer -> Textures -> Int -> Game -> IO ()
renderGame r t winHeight g = do
  print g
  let (Bird pos _) = bird g
  renderTexture r (bird1T t) (P (V2 (150 - 34 `div` 2) (round pos)))

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
             <*> loadTexture r "assets/pipeDown.png"
             <*> loadTexture r "assets/PipeUp.png"
             <*> loadTexture r "assets/sky.png"

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> IO ()
renderTexture r (Texture t size) xy =
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy size)


animate :: Text                  -- ^ window title
        -> Int                   -- ^ window width in pixels
        -> Int                   -- ^ window height in pixels
        -> SF WinInput WinOutput -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) renderConf

    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

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
              renderGame renderer textures winHeight obj
              SDL.present renderer
          return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    -- SDL.destroyTexture $ getSDLTexture texture -- FIXME
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where windowConf = SDL.defaultWindow
             { SDL.windowInitialSize = V2 (fromIntegral winWidth) (fromIntegral winHeight) }
          renderConf = SDL.RendererConfig
             { SDL.rendererType = SDL.AcceleratedVSyncRenderer
             , SDL.rendererTargetTexture = True
             }
