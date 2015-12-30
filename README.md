# Flappy Haskell

Flappy Bird implementation made with SDL2 and FRP (Yampa).

![](bird.gif)

## Installation

You will need some system libraries:
 - SDL2
 - SDL2_Image
 - SDL2_Mixer (with libvorbis/Ogg support enabled!)

Don't forget to get the submodules:

```bash
$ git clone --recursive https://github.com/Rydgel/flappy-haskell
```

```bash
$ stack build
$ stack exec flappy-haskell
```

### Installation on OS X

```bash
$ brew install sdl2
$ brew install sdl2_image
$ brew install sdl2_mixer --with-libvorbis

$ stack build
$ stack exec flappy-haskell
```
