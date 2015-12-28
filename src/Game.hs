{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Game where

import           FRP.Yampa
import           Prelude             hiding (init)

import           Input
import           Types


birdX :: Double
birdX = 75.0

winHeight :: Double
winHeight = 600.0

winWidth :: Double
winWidth = 276.0

groundHeight :: Double
groundHeight = 112.0

pipeGap :: Double
pipeGap = 100.0

pipeWidth :: Double
pipeWidth = 52.0

birdWidth :: Double
birdWidth = 34.0

birdHeight :: Double
birdHeight = 24.0

fallingBird :: Bird -> SF a Bird
fallingBird (Bird y0 v0 s0) = proc _ -> do
  v <- imIntegral v0 -< 250                        -- ^ velocity
  y <- imIntegral y0 -< (v*2)                      -- ^ position
  s <- imIntegral s0 -< 15                         -- ^ to handle bird sprite animation state
  p <- time >>^ ((6 *) . sin . ((2 * pi) *)) -< () -- ^ this will make the bird fly more "naturally"
  returnA -< Bird (y+p) v s

flappingBird :: Bird -> SF AppInput Bird
flappingBird bird0 = switch sf cont
  where sf = proc input -> do
           b <- fallingBird bird0 -< ()
           flap <- flapTrigger -< input
           returnA -< (b, flap `tag` b)
        cont (Bird y _ s) = flappingBird $ Bird y flapVelocity s

movingSky :: Sky -> SF a Sky
movingSky (Sky x0) = proc _ -> do
  x <- imIntegral x0 -< -20
  returnA -< Sky x

movingGround :: Ground -> SF a Ground
movingGround (Ground x0) = proc _ -> do
  x <- imIntegral x0 -< -100
  returnA -< Ground x

movingPipes :: RandomGen g => g -> Pipes -> SF a Pipes
movingPipes rng (Pipes pu pd p0) = switch sf (\_ ->
    let (newPipe,rng') = genRandPipes rng
    in  movingPipes rng' newPipe)
  where sf = proc _ -> do
           p <- imIntegral p0 -< -100
           respawn <- edge -< p < -50
           returnA -< (Pipes pu pd p, respawn)

genRandPipes :: RandomGen g => g -> (Pipes,g)
genRandPipes rng =
  let (pipeTop, rng') = randomR (50.0,320.0) rng
  in  (Pipes pipeTop (winHeight-(pipeTop+pipeGap)) 300.0, rng')

gameSession :: RandomGen g => g -> SF AppInput Game
gameSession rng = proc input -> do
  b <- flappingBird initBird -< input
  s <- movingSky initSky -< ()
  g <- movingGround initGround -< ()
  p <- movingPipes rng initPipes -< ()
  t <- time -< ()
  scr <- time >>^ (round . (/ 3.55)) -< ()
  returnA -< Game b s g p scr t

game :: RandomGen g => g -> SF AppInput Game
game rng = switch sf $ const $ game rng
  where sf = proc input -> do
           gameState <- gameSession rng -< input
           gameOver <- edge -< checkCollision gameState
           returnA -< (gameState, gameOver)

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

flapTrigger :: SF AppInput (Event ())
flapTrigger = proc input -> do
  mouseTap <- lbp -< input
  spacebarTap <- keyPressed ScancodeSpace -< input
  returnA -< mouseTap `lMerge` spacebarTap

checkCollision :: Game -> Bool
checkCollision Game{..} =
  or [ collide (pipeHeight, pipeHeight)
     , collide (winHeight, winHeight - pipeHeight - pipeGap + birdHeight)
     , birdY >= winHeight - groundHeight - birdHeight
     , birdY <= 0] -- upper sky limit
  where collide (y2, h2) = and [ birdX + birdWidth  > pipeX
                               , birdX              < pipeX + pipeWidth
                               , birdY              > y2 - h2
                               , birdY - birdHeight < y2 ]
        birdY = birdPos bird
        pipeX = pipePos pipes
        pipeHeight = pipeUp pipes
