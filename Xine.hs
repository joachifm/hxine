-- |
-- Module      : Xine
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- A simple binding to xine-lib.
--
-- Example usage:
--
-- > import qualified Xine
--
-- > main = do
--
-- >     h <- Xine.open
--
-- >     Xine.openStream h "track.mp3"
--
-- >     Xine.play h

module Xine (
    -- * Configuration
    XineConf(..), defaultConf,
    -- * Types
    VisualType(..), MRL,
    -- * Handle
    XineHandle, open, openWith, close, isClosed,
    -- * Playback
    openStream, play, stop, pause
    ) where

import Xine.Foreign

import Control.Concurrent.MVar
import Control.Monad (unless, when)

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | Xine configuration.
data XineConf = XineConf
    { audioDriver :: Maybe String
      -- ^ Audio driver. Use 'Nothing' for
      -- auto-detection.
    , videoDriver :: Maybe String
      -- ^ Video driver. Use 'Nothing' for
      -- auto-detection.
    , visualType :: VisualType
      -- ^ Video output type. Use 'None' to disable video output.
    }

-- | Default configuration.
defaultConf :: XineConf
defaultConf = XineConf
    { audioDriver = Nothing
    , videoDriver = Nothing
    , visualType = None
    }

------------------------------------------------------------------------------
-- Handle
------------------------------------------------------------------------------

data HandleState = Closed | Open deriving Eq

data XineHandle_ = XineHandle_
    { hEngine :: !Engine
    , hAudioPort :: !AudioPort
    , hVideoPort :: !VideoPort
    , hStream :: !Stream
    , hState :: !HandleState
    }

-- | A xine-lib handle.
newtype XineHandle = XineHandle (MVar XineHandle_)

-- | Test whether the handle is closed.
isClosed :: XineHandle -> IO Bool
isClosed (XineHandle hv) = withMVar hv $ \h -> return (hState h == Closed)

-- A helper for functions using the xine-handle wrapper.
withXineHandle :: XineHandle -> (XineHandle_ -> IO a) -> IO a
withXineHandle h@(XineHandle hv) f = do
    closed <- isClosed h
    when closed (fail "XineHandle is closed")
    withMVar hv $ f

-- | Open a new Xine handle using the default configuration.
open :: IO XineHandle
open = openWith defaultConf

-- | Open a new Xine handle using the supplied 'XineConfig'.
openWith :: XineConf -> IO XineHandle
openWith conf = do
    engine <- xine_new
    xine_init engine

    ap <- maybe (fail "Failed to open the audio driver") return =<<
          xine_open_audio_driver engine (audioDriver conf)
    vp <- maybe (fail "Failed to open the video driver") return =<<
          xine_open_video_driver engine (videoDriver conf) (visualType conf)
    st <- maybe (fail "Failed to open a new stream") return =<<
          xine_stream_new engine ap vp

    h_ <- newMVar $ XineHandle_ engine ap vp st Open
    return $ XineHandle h_

-- | Close Xine handle. The handle is invalid after this.
close :: XineHandle -> IO ()
close h@(XineHandle hv) = do
    withXineHandle h $ \h_ -> do
        xine_close (hStream h_)
        xine_close_audio_driver (hEngine h_) (hAudioPort h_)
        xine_close_video_driver (hEngine h_) (hVideoPort h_)
        xine_exit (hEngine h_)
    modifyMVar_ hv $ \x -> return x { hState = Closed }

------------------------------------------------------------------------------
-- Playback
------------------------------------------------------------------------------

-- | Open a URI for playback.
openStream :: XineHandle -> MRL -> IO ()
openStream h uri = withXineHandle h $ \h_ -> do
    ret <- xine_open (hStream h_) uri
    unless (ret == 1) (fail "Failed to open URI")

-- | Start playback.
play :: XineHandle -> IO ()
play h = withXineHandle h $ \h_ -> do
    ret <- xine_play (hStream h_) 0 0
    unless (ret == 1) (fail "Failed to start playback")

-- | Stop playback.
stop :: XineHandle -> IO ()
stop h = withXineHandle h $ \h_ -> xine_stop (hStream h_)

-- | Toggle pause.
pause :: XineHandle -> IO ()
pause h = withXineHandle h $ \h_ -> do
    s <- xine_get_param (hStream h_) Speed :: IO Speed
    let speed | s == Pause = Normal
              | otherwise  = Pause
    xine_set_param (hStream h_) Speed speed
