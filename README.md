# About
A Haskell FFI binding to [xine-lib], a multimedia playback engine.

[xine-lib]: http://xine-project.org/

# Getting

* `git clone git://github.com/joachifm/hxine.git`

# Dependencies

* [xine-lib]
* [c2hs]

[c2hs]: http://www.cse.unsw.edu.au/~chak/haskell/c2hs/

# Building

Using [cabal-install], do

    cd hxine
    cabal install

[cabal-install]: http://hackage.haskell.org/package/cabal-install/

# Usage

To play an audio file, simply do:

    > import qualified Xine
    > main = do
    >     h <- Xine.open
    >     sid <- Xine.openStream h "track.ogg"
    >     Xine.play h sid

Note that Xine defaults to auto-detecting audio and video drivers.

To configure xine before using it, for example to play video files, use
`openWith` and supply a `XineConf` value:

    > import qualified Xine
    > main = do
    >     h <- Xine.openWith defaultConf { visualType = X11 }
    >     sid <- Xine.openStream h "video.ogm"
    >     Xine.play h sid

See the API documentation (produced by `cabal haddock`) for more information.

If you wish to implement your own higher-level interface to xine-lib,
you may use the low-level FFI binding by

    > import Xine.Foreign

# Author
Joachim Fasting \<joachim.fasting@gmail.com\>

# Licence
LGPL version 2.1 (see COPYING in the source distribution)
