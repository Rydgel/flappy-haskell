{-# LANGUAGE OverloadedStrings #-}

module Rendering (animate) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Text           (Text)
import           Data.Word
import           Foreign.C.Types
import           FRP.Yampa
import           Data.List           (genericLength)
import           Linear              hiding (identity)
import           Linear.Affine
import           Prelude             hiding (init)
import           SDL                 (($=))
import qualified SDL
import qualified Data.HashMap.Strict as M

import           Types
import           Game
import           Audio
import           Graphics


data Textures = Textures { bird1T    :: !Texture
                         , bird2T    :: !Texture
                         , bird3T    :: !Texture
                         , bird4T    :: !Texture
                         , landT     :: !Texture
                         , pipeDownT :: !Texture
                         , pipeUpT   :: !Texture
                         , pipeT     :: !Texture
                         , skyT      :: !Texture
                         }

type DigitsTextures = M.HashMap Char Texture

data SFX = SFX { dieA    :: !Audio
               , hitA    :: !Audio
               , pointA  :: !Audio
               , swooshA :: !Audio
               , wingA   :: !Audio
               }

backgroundColor :: V4 Word8
backgroundColor = V4 55 201 215 maxBound

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

destroyTextures :: Textures -> IO ()
destroyTextures ts = do
  SDL.destroyTexture $ getSDLTexture $ bird1T ts
  SDL.destroyTexture $ getSDLTexture $ bird2T ts
  SDL.destroyTexture $ getSDLTexture $ bird3T ts
  SDL.destroyTexture $ getSDLTexture $ bird4T ts
  SDL.destroyTexture $ getSDLTexture $ landT ts
  SDL.destroyTexture $ getSDLTexture $ pipeDownT ts
  SDL.destroyTexture $ getSDLTexture $ pipeUpT ts
  SDL.destroyTexture $ getSDLTexture $ pipeT ts
  SDL.destroyTexture $ getSDLTexture $ skyT ts

loadDigitsTextures :: SDL.Renderer -> IO DigitsTextures
loadDigitsTextures r = mapM (loadTexture r) $
  M.fromList [ ('0', "assets/font_big_0.png")
             , ('1', "assets/font_big_1.png")
             , ('2', "assets/font_big_2.png")
             , ('3', "assets/font_big_3.png")
             , ('4', "assets/font_big_4.png")
             , ('5', "assets/font_big_5.png")
             , ('6', "assets/font_big_6.png")
             , ('7', "assets/font_big_7.png")
             , ('8', "assets/font_big_8.png")
             , ('9', "assets/font_big_9.png")
             ]

destroyDigitsTextures :: DigitsTextures -> IO ()
destroyDigitsTextures = mapM_ (SDL.destroyTexture . getSDLTexture)

loadAudios :: IO SFX
loadAudios = SFX
         <$> loadAudio "assets/sounds/sfx_die.ogg"
         <*> loadAudio "assets/sounds/sfx_hit.ogg"
         <*> loadAudio "assets/sounds/sfx_point.ogg"
         <*> loadAudio "assets/sounds/sfx_swooshing.ogg"
         <*> loadAudio "assets/sounds/sfx_wing.ogg"

destroyAudios :: SFX -> IO ()
destroyAudios as = do
  destroyAudio $ unAudio $ dieA as
  destroyAudio $ unAudio $ hitA as
  destroyAudio $ unAudio $ pointA as
  destroyAudio $ unAudio $ swooshA as
  destroyAudio $ unAudio $ wingA as

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
renderBird r t b = renderTextureRotated r birdSprite cxy angleBird
  where
    cxy        = P (V2 75 posBird)
    posBird    = round $ birdPos b
    stateBird  = round $ birdState b :: Int
    angleBird  = birdAngleFromVelocity $ birdVel b
    birdSprite = birdSpriteFromState (stateBird `mod` 4) t

renderPipes :: SDL.Renderer -> Textures -> CInt -> Pipes -> IO ()
renderPipes r t wh p = do
  let posX          = round $ pipePos p
      topPipeHeight = round $ pipeUp p
      botPipeHeight = round $ pipeDown p
      pipeUpY       = wh - botPipeHeight
      pipeDownY     = topPipeHeight
      pipeDownFills = [0..pipeDownY]
      pipeUpFills   = [(pipeUpY+26)..wh]
  -- pipe down
  renderTexture r (pipeDownT t) (P (V2 posX pipeDownY))
  renderRepeatedTextureY r (pipeT t) posX pipeDownFills
  -- pipe up
  renderTexture r (pipeUpT t) (P (V2 posX pipeUpY))
  renderRepeatedTextureY r (pipeT t) posX pipeUpFills

renderDigit :: SDL.Renderer -> DigitsTextures -> Char -> Point V2 CInt -> IO ()
renderDigit r dt c = renderTexture r $ (M.!) dt c

renderScore :: SDL.Renderer -> DigitsTextures -> Int -> IO ()
renderScore r dt scoreInt = do
  let stringScore = show scoreInt
      totalWidth  = 28.0 * genericLength stringScore
      screenW     = 276.0 :: Double
      start       = round $ (screenW - totalWidth) / 2
  foldM_ (\z c -> renderDigit r dt c (P (V2 z 25)) >> return (z+28)) start stringScore

renderDisplay :: SDL.Renderer -> Textures -> DigitsTextures -> CInt -> Game -> IO ()
renderDisplay r t dt winHeight g = do
  -- print g
  -- moving sky
  renderRepeatedTexture r (skyT t) posSky (winHeight-112)
  -- Rendering pipes
  renderPipes r t winHeight (pipes g)
  -- Moving ground
  renderRepeatedTexture r (landT t) posGround winHeight
  -- The animated bird
  renderBird r t (bird g)
  -- the score
  renderScore r dt (score g)
  where
    posGround = round $ groundPos $ ground g
    posSky = round $ skyPos $ sky g

renderSounds :: SFX -> Game -> IO ()
renderSounds as g = do
  when (birdVel (bird g) == flapVelocity) $
    playFile (wingA as) 1
  when (checkCollision g) $
    playFile (dieA as) 3
  let pipeCoord = pipePos (pipes g)
  when (pipeCoord >= 75.0 && pipeCoord <= 76.0) $
    playFile (pointA as) 2

animate :: Text                  -- ^ window title
        -> Int                   -- ^ window width in pixels
        -> Int                   -- ^ window height in pixels
        -> SF WinInput WinOutput -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDL.HintRenderScaleQuality $= SDL.ScaleBest
    initAudio
    audios <- loadAudios
    window <- SDL.createWindow title windowConf
    SDL.showWindow window
    renderer <- SDL.createRenderer window (-1) renderConf
    SDL.rendererDrawColor renderer $= backgroundColor
    textures <- loadTextures renderer
    digitsTextures <- loadDigitsTextures renderer

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
          currentTime <- SDL.time
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          mEvent <- SDL.pollEvent
          return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (gameState, shouldExit) = do
          when changed $ do
              SDL.clear renderer
              renderDisplay renderer textures digitsTextures (fromIntegral winHeight) gameState
              renderSounds audios gameState
              SDL.present renderer
          return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    destroyTextures textures
    destroyDigitsTextures digitsTextures
    destroyAudios audios
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
