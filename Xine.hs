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

module Xine (
    -- * Handle
    XineHandle, open, close,
    -- * Playback
    openStream, play, stop, pause
    ) where

import Xine.Foreign

import Control.Monad (unless)
import Foreign
import Foreign.C

-- | A xine-lib handle.
data XineHandle = XineHandle
    { hEngine :: !(Ptr Engine)
    , hAudioPort :: !(Ptr AudioPort)
    , hVideoPort :: !(Ptr VideoPort)
    , hStream :: !(Ptr Stream)
    }

-- | Open a new Xine handle.
open :: IO XineHandle
open = do
    engine <- xine_new
    xine_init engine

    ap <- xine_open_audio_driver engine nullPtr nullPtr
    vp <- xine_open_video_driver engine nullPtr visual_none nullPtr

    st <- xine_stream_new engine ap vp

    return $ XineHandle engine ap vp st

-- | Close Xine handle. The handle is invalid after this.
close :: XineHandle -> IO ()
close h = do
    xine_close (hStream h)
    xine_close_audio_driver (hEngine h) (hAudioPort h)
    xine_close_video_driver (hEngine h) (hVideoPort h)
    xine_exit (hEngine h)

-- | Open a URI for playback.
openStream :: XineHandle -> String -> IO ()
openStream h uri = do
    ret <- withCString uri $ \s -> xine_open (hStream h) s
    unless (ret == 1) (fail "Failed to open URI")

-- | Start playback.
play :: XineHandle -> IO ()
play h = do
    ret <- xine_play (hStream h) 0 0
    unless (ret == 1) (fail "Failed to start playback")

-- | Stop playback.
stop :: XineHandle -> IO ()
stop h = xine_stop (hStream h)

-- | Toggle pause
pause :: XineHandle -> IO ()
pause h = do
    s <- xine_get_param (hStream h) param_speed
    let speed | unSpeed speed_normal == s = speed_pause
              | otherwise                 = speed_normal
    xine_set_param (hStream h) param_speed (unSpeed speed)
