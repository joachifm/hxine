{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Xine.Foreign
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- A simple binding to xine-lib. Low-level bindings.

module Xine.Foreign (
    -- * Version information
    xine_get_version_string, xine_get_version, xine_check_version,
    -- * Global engine handling
    Engine, AudioPort, VideoPort, VisualType(..),
    xine_new, xine_init, xine_open_audio_driver, xine_open_video_driver,
    xine_close_audio_driver, xine_close_video_driver, xine_exit,
    -- * Stream handling
    Stream, StreamParam(..), Speed(..), NormalSpeed(..), Zoom(..),
    AspectRatio(..), Mrl, EngineParam(..),
    xine_stream_new, xine_open, xine_play, xine_stop, xine_close,
    xine_engine_set_param, xine_engine_get_param,
    xine_set_param, xine_get_param,
    -- * Information retrieval
    EngineStatus(..), XineError(..),
    xine_get_error, xine_get_status
    ) where

import Control.Monad (liftM)
import Foreign
import Foreign.C

#include <xine.h>

-- Define the name of the dynamic library that has to be loaded before any of
-- the external C functions may be invoked.
-- The prefix declaration allows us to refer to identifiers while omitting the
-- prefix. Prefix matching is case insensitive and any underscore characters
-- between the prefix and the stem of the identifiers are also removed.
{#context lib="xine" prefix="xine"#}

-- Opaque types used for structures that are never dereferenced on the
-- Haskell side.
{#pointer *xine_t as Engine foreign newtype#}
{#pointer *xine_audio_port_t as AudioPort foreign newtype#}
{#pointer *xine_video_port_t as VideoPort foreign newtype#}
{#pointer *xine_stream_t as Stream foreign newtype#}

newtype Data = Data (Ptr Data)

-- | Media Resource Locator.
type Mrl = String

------------------------------------------------------------------------------
-- Marshalling
------------------------------------------------------------------------------

int2bool = (/= 0)

cint2enum :: Enum a => CInt -> a
cint2enum = toEnum . fromIntegral

enum2cint :: Enum a => a -> CInt
enum2cint = fromIntegral . fromEnum

peekInt = liftM fromIntegral . peek

maybeForeignPtr_ c x | x == nullPtr = return Nothing
                     | otherwise    = (Just . c) `liftM` newForeignPtr_ x

peekEngine = liftM Engine . newForeignPtr_

peekAudioPort = maybeForeignPtr_ AudioPort

peekVideoPort = maybeForeignPtr_ VideoPort

peekStream = maybeForeignPtr_ Stream

withData f = f nullPtr

withMaybeString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeString Nothing f  = f nullPtr
withMaybeString (Just s) f = withCString s f

------------------------------------------------------------------------------
-- Version information
------------------------------------------------------------------------------

-- | Get xine-lib version string.
--
-- Header declaration:
--
-- const char *xine_get_version_string (void)
{#fun pure xine_get_version_string {} -> `String' peekCString*#}

-- | Get version as a triple: major, minor, sub
--
-- Header declaration:
--
-- void xine_get_version (int *major, int *minor, int *sub)
{#fun pure xine_get_version
 {alloca- `Int' peekInt*,
  alloca- `Int' peekInt*,
  alloca- `Int' peekInt*} -> `()'#}

-- | Compare given version to xine-lib version (major, minor, sub).
--
-- Header declaration:
--
-- int xine_check_version (int major, int minor, int sub)
--
-- returns 1 if compatible, 0 otherwise.
{#fun pure xine_check_version
 {fromIntegral `Int',
  fromIntegral `Int',
  fromIntegral `Int'} -> `Bool' int2bool#}

------------------------------------------------------------------------------
-- Global engine handling
------------------------------------------------------------------------------

-- | Valid visual types
{#enum define VisualType {XINE_VISUAL_TYPE_NONE as None
                         ,XINE_VISUAL_TYPE_X11 as X11
                         ,XINE_VISUAL_TYPE_X11_2 as X11_2
                         ,XINE_VISUAL_TYPE_AA as AA
                         ,XINE_VISUAL_TYPE_FB as FB
                         ,XINE_VISUAL_TYPE_GTK as GTK
                         ,XINE_VISUAL_TYPE_DFB as DFB
                         ,XINE_VISUAL_TYPE_PM as PM
                         ,XINE_VISUAL_TYPE_DIRECTX as DirectX
                         ,XINE_VISUAL_TYPE_CACA as CACA
                         ,XINE_VISUAL_TYPE_MACOSX as MacOSX
                         ,XINE_VISUAL_TYPE_XCB as XCB
                         ,XINE_VISUAL_TYPE_RAW as Raw
                         }#}

-- | Pre-init the Xine engine.
--
-- Header declaration:
--
-- xine_t *xine_new (void)
{#fun unsafe xine_new {} -> `Engine' peekEngine*#}

-- | Post-init the Xine engine.
--
-- Header declaration:
--
-- void xine_init (xine_t *self)
{#fun unsafe xine_init {withEngine* `Engine'} -> `()'#}

-- | Initialise audio driver.
--
-- Header declaration:
--
-- xine_audio_port_t *xine_open_audio_driver (xine_t *self, const char *id,
--                        void *data)
--
-- id: identifier of the driver, may be NULL for auto-detection
--
-- data: special data struct for ui/driver communication
--
-- May return NULL if the driver failed to load.
{#fun unsafe xine_open_audio_driver
 {withEngine* `Engine'
 ,withMaybeString* `(Maybe String)'
 ,withData- `Data'} -> `(Maybe AudioPort)' peekAudioPort*#}

-- | Initialise video driver.
--
-- Header declaration:
--
-- xine_video_port_t *xine_open_video_driver (xine_t *self, const char *id,
--                        int visual, void *data)
--
-- id: identifier of the driver, may be NULL for auto-detection
--
-- data: special data struct for ui/driver communication
--
-- visual : video driver flavor selector
--
-- May return NULL if the driver failed to load.
{#fun unsafe xine_open_video_driver
 {withEngine* `Engine'
 ,withMaybeString* `(Maybe String)'
 ,enum2cint `VisualType'
 ,withData- `Data'} -> `(Maybe VideoPort)' peekVideoPort*#}

-- | Close audio port.
--
-- Header declaration:
--
-- void xine_close_audio_driver (xine_t *self, xine_audio_port_t *driver)
{#fun unsafe xine_close_audio_driver
 {withEngine* `Engine'
 ,withAudioPort* `AudioPort'} -> `()'#}

-- | Close video port.
--
-- Header declaration:
--
-- void xine_close_video_driver (xine_t *self, xine_video_port_t *driver)
{#fun unsafe xine_close_video_driver
 {withEngine* `Engine'
 ,withVideoPort* `VideoPort'} -> `()'#}

-- | Free all resources, close all plugins, close engine.
--
-- Header declaration:
--
-- void xine_exit (xine_t *self)
{#fun unsafe xine_exit {withEngine* `Engine'} -> `()'#}

------------------------------------------------------------------------------
-- Stream handling
------------------------------------------------------------------------------

-- | Engine parameter enumeration.
{#enum define EngineParam
           {XINE_ENGINE_PARAM_VERBOSITY as EngineVerbosity}#}

-- | Stream parameter enumeration.
{#enum define StreamParam
           {XINE_PARAM_SPEED as Speed
           ,XINE_PARAM_AV_OFFSET as AvOffset
           ,XINE_PARAM_AUDIO_CHANNEL_LOGICAL as AudioChannelLogical
           ,XINE_PARAM_SPU_CHANNEL as SpuChannel
           ,XINE_PARAM_AUDIO_VOLUME as AudioVolume
           ,XINE_PARAM_AUDIO_MUTE as AudioMute
           ,XINE_PARAM_AUDIO_COMPR_LEVEL as AudioComprLevel
           ,XINE_PARAM_AUDIO_REPORT_LEVEL as AudioReportLevel
           ,XINE_PARAM_VERBOSITY as Verbosity
           ,XINE_PARAM_SPU_OFFSET as SpuOffset
           ,XINE_PARAM_IGNORE_VIDEO as IgnoreVideo
           ,XINE_PARAM_IGNORE_AUDIO as IgnoreAudio
           ,XINE_PARAM_BROADCASTER_PORT as BroadcasterPort
           ,XINE_PARAM_METRONOM_PREBUFFER as MetronomPrebuffer
           ,XINE_PARAM_EQ_30HZ as Eq30Hz
           ,XINE_PARAM_EQ_60HZ as Eq60Hz
           ,XINE_PARAM_EQ_125HZ as Eq125Hz
           ,XINE_PARAM_EQ_500HZ as Eq500Hz
           ,XINE_PARAM_EQ_1000HZ as Eq1000Hz
           ,XINE_PARAM_EQ_2000HZ as Eq2000Hz
           ,XINE_PARAM_EQ_4000HZ as Eq4000Hz
           ,XINE_PARAM_EQ_8000HZ as Eq8000Hz
           ,XINE_PARAM_EQ_16000HZ as Eq16000Hz
           ,XINE_PARAM_AUDIO_CLOSE_DEVICE as AudioCloseDevice
           ,XINE_PARAM_AUDIO_AMP_MUTE as AmpMute
           ,XINE_PARAM_FINE_SPEED as FineSpeed
           ,XINE_PARAM_EARLY_FINISHED_EVENT as EarlyFinishedEvent
           ,XINE_PARAM_GAPLESS_SWITCH as GaplessSwitch
           ,XINE_PARAM_DELAY_FINISHED_EVENT as DelayFinishedEvent
           ,XINE_PARAM_VO_DEINTERLACE as Deinterlace
           ,XINE_PARAM_VO_ASPECT_RATIO as AspectRatio
           ,XINE_PARAM_VO_HUE as Hue
           ,XINE_PARAM_VO_SATURATION as Saturation
           ,XINE_PARAM_VO_CONTRAST as Contrast
           ,XINE_PARAM_VO_BRIGHTNESS as Brightness
           ,XINE_PARAM_VO_ZOOM_X as ZoomX
           ,XINE_PARAM_VO_ZOOM_Y as ZoomY
           ,XINE_PARAM_VO_PAN_SCAN as PanScan
           ,XINE_PARAM_VO_TVMODE as TvMode
           ,XINE_PARAM_VO_WINDOW_WIDTH as WindowWidth
           ,XINE_PARAM_VO_WINDOW_HEIGHT as WindowHeight
           ,XINE_PARAM_VO_CROP_LEFT as CropLeft
           ,XINE_PARAM_VO_CROP_RIGHT as CropRight
           ,XINE_PARAM_VO_CROP_TOP as CropTop
           ,XINE_PARAM_VO_CROP_BOTTOM as CropBottom
           }#}

-- | Values for XINE_PARAM_SPEED parameter.
{#enum define Speed
           {XINE_SPEED_PAUSE as Pause
           ,XINE_SPEED_SLOW_4 as Slow4
           ,XINE_SPEED_SLOW_2 as Slow2
           ,XINE_SPEED_NORMAL as Normal
           ,XINE_SPEED_FAST_2 as Fast2
           ,XINE_SPEED_FAST_4 as Fast4
           }#}

deriving instance Eq Speed

-- | Value for XINE_PARAM_FINE_SPEED
{#enum define NormalSpeed
           {XINE_FINE_SPEED_NORMAL as NormalSpeed}#}

-- | Values for XINE_PARAM_VO_ZOOM_
{#enum define Zoom
           {XINE_VO_ZOOM_STEP as ZoomStep
           ,XINE_VO_ZOOM_MAX as ZoomMax
           ,XINE_VO_ZOOM_MIN as ZoomMin}#}

-- | Values for XINE_PARAM_VO_ASPECT_RATIO
{#enum define AspectRatio
           {XINE_VO_ASPECT_AUTO as AspectAuto
           ,XINE_VO_ASPECT_SQUARE as AspectSquare
           ,XINE_VO_ASPECT_4_3 as Aspect43
           ,XINE_VO_ASPECT_ANAMORPHIC as AspectAnamorphic
           ,XINE_VO_ASPECT_DVB as AspectDvb
           ,XINE_VO_ASPECT_NUM_RATIOS as AspectNumRatios
           }#}

-- | Create a new stream for media playback.
--
-- Header declaration:
--
-- xine_stream_t *xine_stream_new (xine_t *self,
--     xine_audio_port *ao, xine_video_port_t *vo)
--
-- Returns xine_stream_t* if OK, NULL on error.
{#fun unsafe xine_stream_new
 {withEngine* `Engine'
 ,withAudioPort* `AudioPort'
 ,withVideoPort* `VideoPort'} -> `(Maybe Stream)' peekStream*#}

-- | Open a stream.
--
-- Header declaration:
--
-- int xine_open (xine_stream_t *stream, const char *mrl)
--
-- Returns 1 if OK, 0 on error.
{#fun unsafe xine_open
 {withStream* `Stream'
 ,withCAString* `Mrl'} -> `Int' fromIntegral#}

-- | Play a stream from a given position.
--
-- Header declaration:
--
-- int xine_play (xine_stream_t *stream, int start_pos, int start_time)
--
-- Returns 1 if OK, 0 on error.
{#fun unsafe xine_play
 {withStream* `Stream'
 ,fromIntegral `Int'
 ,fromIntegral `Int'} -> `Int' fromIntegral#}

-- | Stop stream playback.
-- The stream stays valid for new 'xine_open' or 'xine_play'.
--
-- Header declaration:
--
-- void xine_stop (xine_stream *stream)
{#fun unsafe xine_stop {withStream* `Stream'} -> `()'#}

-- | Free all stream-related resources.
-- The stream stays valid for new xine_open.
--
-- Header declaration:
--
-- void xine_close (xine_stream_t *stream)
{#fun unsafe xine_close {withStream* `Stream'} -> `()'#}

-- | Set engine parameter.
--
-- Header declaration:
--
-- void xine_engine_set_param (xine_t *self, int param, int value)
{#fun unsafe xine_engine_set_param
 {withEngine* `Engine'
 ,enum2cint `EngineParam'
 ,fromIntegral `Int'} -> `()'#}

-- | Get engine parameter.
--
-- Header declaration:
--
-- int xine_engine_get_param(xine_t *self, int param)
{#fun unsafe xine_engine_get_param
 {withEngine* `Engine'
 ,enum2cint `EngineParam'} -> `Int' fromIntegral#}

-- | Set stream parameter.
--
-- Header declaration:
--
-- void xine_set_param (xine_stream_t *stream, int param, int value)
{#fun unsafe xine_set_param
 `(Enum a)' =>
 {withStream* `Stream'
 ,enum2cint `StreamParam'
 ,enum2cint `a'} -> `()'#}

-- | Get stream parameter.
--
-- Header declaration:
--
-- int xine_get_param (xine_stream_t *stream, int param)
{#fun unsafe xine_get_param
 `(Enum a)' =>
 {withStream* `Stream'
 ,enum2cint `StreamParam'} -> `a' cint2enum#}

------------------------------------------------------------------------------
-- Information retrieval
------------------------------------------------------------------------------

-- | Engine status codes.
{#enum define EngineStatus
           {XINE_STATUS_IDLE as Idle
           ,XINE_STATUS_STOP as Stopped
           ,XINE_STATUS_PLAY as Playing
           ,XINE_STATUS_QUIT as Quitting}#}

deriving instance Eq EngineStatus
deriving instance Show EngineStatus

-- | Xine error codes.
{#enum define XineError
           {XINE_ERROR_NONE as NoError
           ,XINE_ERROR_NO_INPUT_PLUGIN as NoInputPlugin
           ,XINE_ERROR_NO_DEMUX_PLUGIN as NoDemuxPlugin
           ,XINE_ERROR_MALFORMED_MRL as MalformedMrl
           ,XINE_ERROR_INPUT_FAILED as InputFailed}#}

deriving instance Eq XineError
deriving instance Show XineError

-- | Return last error.
--
-- Header declaration:
--
-- int xine_get_error (xine_stream_t *stream)
{#fun unsafe xine_get_error
 {withStream* `Stream'} -> `XineError' cint2enum#}

-- | Get current xine engine status.
--
-- int xine_get_status (xine_stream_t *stream)
{#fun unsafe xine_get_status
 {withStream* `Stream'} -> `EngineStatus' cint2enum#}
