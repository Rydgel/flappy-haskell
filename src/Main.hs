{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FRP.Yampa
import           Prelude   hiding (init)

import           Graphics
import           Input
import           Types


fallingBird :: Bird -> SF a Bird
fallingBird (Bird y0 v0) = proc _ -> do
  v <- imIntegral v0 -< 250
  y <- imIntegral y0 -< v
  returnA -< Bird y v

flappingBird :: Bird -> SF AppInput Bird
flappingBird bird0 = switch sf cont
    where sf = proc input -> do
              b <- fallingBird bird0 -< ()
              flap <- flapTrigger -< input
              returnA -< (b, flap `tag` b)
          cont (Bird y v) = flappingBird (Bird y (v - 250))

movingSky :: Sky -> SF a Sky
movingSky (Sky x0) = proc _ -> do
  x <- imIntegral x0 -< -50
  returnA -< Sky x

movingGround :: Ground -> SF a Ground
movingGround (Ground x0) = proc _ -> do
  x <- imIntegral x0 -< -100
  returnA -< Ground x

checkCollision :: Game -> Bool
checkCollision _ = True

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

main :: IO ()
main = animate "Flappy Haskell" 300 600 (parseWinInput >>> (game &&& handleExit))
