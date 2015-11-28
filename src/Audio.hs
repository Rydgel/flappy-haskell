{-# LANGUAGE OverloadedStrings #-}

module Audio
    (Music(..),
     Audio(..),
     initAudio,
     loadAudio,
     destroyAudio,
     loadMusic,
     playMusic,
     playFile,
     stopMusic,
     musicPlaying) where

import           Control.Concurrent
import           Control.Monad
import qualified SDL.Mixer
import qualified SDL.Raw.Mixer as RawMix

data Music = Music { musicName :: String, unMusic :: SDL.Mixer.Music }
data Audio = Audio { audioName :: String, unAudio :: SDL.Mixer.Chunk }

-- | Initialize the audio subsystem.
--
-- Audio quality and number of channels are fixed (16).
initAudio :: IO ()
initAudio = void $ do
  SDL.Mixer.initialize [SDL.Mixer.InitOGG]
  _result <- RawMix.openAudio 44100 RawMix.AUDIO_S16SYS 2 4096
  SDL.Mixer.reserveChannels 16

-- | Load a music file, returning a 'Music' if loaded successfully.
loadMusic :: String -> IO Music
loadMusic fp = do
  music <- SDL.Mixer.load fp
  return $ Music fp music

-- | Free memory
destroyAudio :: SDL.Mixer.Chunk -> IO ()
destroyAudio = SDL.Mixer.free

-- | Play music in a loop at max volume.
playMusic :: Music -> IO ()
playMusic m = do
  SDL.Mixer.setMusicVolume 100
  SDL.Mixer.playMusic SDL.Mixer.Forever (unMusic m)

-- | Stop playing music
stopMusic :: IO ()
stopMusic = SDL.Mixer.haltMusic

-- | Is music playing?
musicPlaying :: IO Bool
musicPlaying = SDL.Mixer.playingMusic

-- | Load an audio file.
loadAudio :: String -> IO Audio
loadAudio fp = do
  audio <- SDL.Mixer.load fp
  return $ Audio fp audio

-- | Play an audio file for the given number of seconds.
--
-- This function spawns a new OS thread. Remember to compile your program
-- with the threaded RTS.
playFile :: Audio -> Int -> IO ()
playFile wav t = void $ forkOS $ do
  _v <- SDL.Mixer.playOn 0 SDL.Mixer.Once (unAudio wav)
  threadDelay (t * 1000)
