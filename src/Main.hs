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
  animate "Flappy Haskell" 276 600 (parseWinInput >>> (game g &&& handleExit))
