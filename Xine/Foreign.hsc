{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module      : Xine.Foreign
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- A simple binding to xine-lib. Low-level bindings.

module Xine.Foreign where

#include <xine.h>

import Foreign
import Foreign.C

data Engine
data AudioPort
data VideoPort
data Stream

------------------------------------------------------------------------------
-- Global engine handling
------------------------------------------------------------------------------

newtype VisualType = VisualType { unVisualType :: CInt }
    deriving (Eq, Show)

#{enum VisualType, VisualType
 ,visual_none = XINE_VISUAL_TYPE_NONE
 ,visual_x11 = XINE_VISUAL_TYPE_X11
 ,visual_x11_2 = XINE_VISUAL_TYPE_X11_2
 ,visual_aa = XINE_VISUAL_TYPE_AA
 ,visual_fb = XINE_VISUAL_TYPE_FB
 ,visual_gtk = XINE_VISUAL_TYPE_GTK
 ,visual_dfb = XINE_VISUAL_TYPE_DFB
 ,visual_pm = XINE_VISUAL_TYPE_PM
 ,visual_directx = XINE_VISUAL_TYPE_DIRECTX
 ,visual_caca = XINE_VISUAL_TYPE_CACA
 ,visual_macosx = XINE_VISUAL_TYPE_MACOSX
 ,visual_xcb = XINE_VISUAL_TYPE_XCB
 ,visual_raw = XINE_VISUAL_TYPE_RAW
 }

foreign import ccall unsafe "xine_new" xine_new :: IO (Ptr Engine)

foreign import ccall unsafe "xine_init" xine_init :: Ptr Engine -> IO ()

foreign import ccall unsafe "xine_open_audio_driver" xine_open_audio_driver
    :: Ptr Engine -> CString -> Ptr CString -> IO (Ptr AudioPort)

foreign import ccall unsafe "xine_open_video_driver" xine_open_video_driver
    :: Ptr Engine -> CString -> VisualType -> Ptr CString -> IO (Ptr VideoPort)

foreign import ccall unsafe "xine_close_audio_driver" xine_close_audio_driver
    :: Ptr Engine -> Ptr AudioPort -> IO ()

foreign import ccall unsafe "xine_close_video_driver" xine_close_video_driver
    :: Ptr Engine -> Ptr VideoPort -> IO ()

foreign import ccall unsafe "xine_exit" xine_exit :: Ptr Engine -> IO ()

------------------------------------------------------------------------------
-- Stream handling
------------------------------------------------------------------------------

newtype StreamParam = StreamParam { unStreamParam :: CInt }
    deriving (Eq, Show)

#{enum StreamParam, StreamParam
 ,param_speed = XINE_PARAM_SPEED
 ,param_av_offset = XINE_PARAM_AV_OFFSET
 ,param_audio_channel_logical = XINE_PARAM_AUDIO_CHANNEL_LOGICAL
 ,param_spu_channel = XINE_PARAM_SPU_CHANNEL
 ,param_audio_volume = XINE_PARAM_AUDIO_VOLUME
 ,param_audio_mute = XINE_PARAM_AUDIO_MUTE
 ,param_audio_compr_level = XINE_PARAM_AUDIO_COMPR_LEVEL
 ,param_audio_amp_level = XINE_PARAM_AUDIO_AMP_LEVEL
 ,param_audio_report_level = XINE_PARAM_AUDIO_REPORT_LEVEL
 ,param_verbosity = XINE_PARAM_VERBOSITY
 ,param_spu_offset = XINE_PARAM_SPU_OFFSET
 ,param_ignore_video = XINE_PARAM_IGNORE_VIDEO
 ,param_ignore_audio = XINE_PARAM_IGNORE_AUDIO
 ,param_ignore_spu = XINE_PARAM_IGNORE_SPU
 ,param_broadcaster_port = XINE_PARAM_BROADCASTER_PORT
 ,param_metronom_prebuffer = XINE_PARAM_METRONOM_PREBUFFER
 ,param_eq_30hz = XINE_PARAM_EQ_30HZ
 ,param_eq_60hz = XINE_PARAM_EQ_60HZ
 ,param_eq_125hz = XINE_PARAM_EQ_125HZ
 ,param_eq_500hz = XINE_PARAM_EQ_500HZ
 ,param_eq_1000hz = XINE_PARAM_EQ_1000HZ
 ,param_eq_2000hz = XINE_PARAM_EQ_2000HZ
 ,param_eq_4000hz = XINE_PARAM_EQ_4000HZ
 ,param_eq_8000hz = XINE_PARAM_EQ_8000HZ
 ,param_eq_16000hz = XINE_PARAM_EQ_16000HZ
 ,param_audio_close_device = XINE_PARAM_AUDIO_CLOSE_DEVICE
 ,param_audio_amp_mute = XINE_PARAM_AUDIO_AMP_MUTE
 ,param_fine_speed = XINE_PARAM_FINE_SPEED
 ,param_early_finished_event = XINE_PARAM_EARLY_FINISHED_EVENT
 ,param_gapless_switch = XINE_PARAM_GAPLESS_SWITCH
 ,param_delay_finished_event = XINE_PARAM_DELAY_FINISHED_EVENT
 }

newtype Speed = Speed { unSpeed :: CInt }
    deriving (Eq, Show)

#{enum Speed, Speed
 ,speed_pause = XINE_SPEED_PAUSE
 ,speed_slow_4 = XINE_SPEED_SLOW_4
 ,speed_slow_2 = XINE_SPEED_SLOW_2
 ,speed_normal = XINE_SPEED_NORMAL
 ,speed_fast_2 = XINE_SPEED_FAST_2
 ,speed_fast_4 = XINE_SPEED_FAST_4
 }

foreign import ccall unsafe "xine_stream_new" xine_stream_new
    :: Ptr Engine -> Ptr AudioPort -> Ptr VideoPort -> IO (Ptr Stream)

-- Returns 1 if OK, 0 on error. Use xine_get_error for details
foreign import ccall unsafe "xine_open" xine_open
    :: Ptr Stream -> CString -> IO CInt

-- Returns 1 if OK, 0 on error.
foreign import ccall unsafe "xine_play" xine_play
    :: Ptr Stream -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "xine_stop" xine_stop :: Ptr Stream -> IO ()

foreign import ccall unsafe "xine_close" xine_close :: Ptr Stream -> IO ()

foreign import ccall unsafe "xine_set_param" xine_set_param
    :: Ptr Stream -> StreamParam -> CInt -> IO ()

foreign import ccall unsafe "xine_get_param" xine_get_param
    :: Ptr Stream -> StreamParam -> IO CInt
