{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FRP.Yampa
import Game
import Input
import Rendering


main :: IO ()
main = animate "Flappy Haskell" 276 600 (parseWinInput >>> (game &&& handleExit))
