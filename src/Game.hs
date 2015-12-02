{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import           FRP.Yampa
import           Prelude   hiding (init)

import           Input
import           Types


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
  x <- imIntegral x0 -< -70
  returnA -< Ground x

gameSession :: SF AppInput Game
gameSession = proc input -> do
  b <- flappingBird initBird -< input
  s <- movingSky initSky -< ()
  g <- movingGround initGround -< ()
  returnA -< Game { bird = b, sky = s, ground = g }

game :: SF AppInput Game
game = switch sf (const game)
  where sf = proc input -> do
              gameState <- gameSession -< input
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
checkCollision _ = False
