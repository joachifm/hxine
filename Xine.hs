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
    XineHandle, open, close, isClosed,
    -- * Playback
    openStream, play, stop, pause
    ) where

import Xine.Foreign

import Control.Concurrent.MVar
import Control.Monad (unless, when)
import Foreign
import Foreign.C

data HandleState = Closed | Open deriving Eq

-- Internal handle structure.
data XineHandle_ = XineHandle_
    { hEngine :: !(Ptr Engine)
    , hAudioPort :: !(Ptr AudioPort)
    , hVideoPort :: !(Ptr VideoPort)
    , hStream :: !(Ptr Stream)
    , hState :: HandleState
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

-- | Open a new Xine handle.
open :: IO XineHandle
open = do
    engine <- xine_new
    xine_init engine

    ap <- xine_open_audio_driver engine nullPtr nullPtr
    vp <- xine_open_video_driver engine nullPtr visual_none nullPtr

    st <- xine_stream_new engine ap vp

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

-- | Open a URI for playback.
openStream :: XineHandle -> String -> IO ()
openStream h uri = withXineHandle h $ \h_ -> do
    ret <- withCString uri $ \s -> xine_open (hStream h_) s
    unless (ret == 1) (fail "Failed to open URI")

-- | Start playback.
play :: XineHandle -> IO ()
play h = withXineHandle h $ \h_ -> do
    ret <- xine_play (hStream h_) 0 0
    unless (ret == 1) (fail "Failed to start playback")

-- | Stop playback.
stop :: XineHandle -> IO ()
stop h = withXineHandle h $ \h_ -> xine_stop (hStream h_)

-- | Toggle pause
pause :: XineHandle -> IO ()
pause h = withXineHandle h $ \h_ -> do
    s <- xine_get_param (hStream h_) param_speed
    let speed | unSpeed speed_normal == s = speed_pause
              | otherwise                 = speed_normal
    xine_set_param (hStream h_) param_speed (unSpeed speed)
