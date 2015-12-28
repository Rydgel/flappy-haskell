{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FRP.Yampa
import           Game
import           Input
import           Rendering
import           System.Random (newStdGen)


main :: IO ()
main = do
  g <- newStdGen
  animate "Flappy Haskell" (round winWidth) (round winHeight) $
    parseWinInput >>> (game g &&& handleExit)
