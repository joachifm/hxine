# About
A Haskell FFI binding to [xine-lib], a multimedia playback engine.

[xine-lib]: http://xine-project.org

# Getting

* `git clone git://github.com/joachifm/hxine.git`

# Building

Using [cabal-install], do

    cd hxine
    cabal install

[cabal-install]: http://hackage.haskell.org/package/cabal-install

# Usage

To play an audio file, simply do:

    > import qualified Xine
    > main = do
    >     h <- Xine.open
    >     Xine.openStream h "track.ogg"
    >     Xine.play h

Note that Xine defaults to auto-detecting audio and video drivers.

To configure xine before using it, for example to play video files, use
`openWith` and supply a `XineConf` value:

    > import qualified Xine
    > main = do
    >     h <- Xine.openWith defaultConf { visualType = X11 }
    >     Xine.openStream h "video.ogm"
    >     Xine.play h

# Author
Joachim Fasting \<joachim.fasting@gmail.com\>

# Licence
LGPL version 2.1 (see COPYING in the source distribution)
