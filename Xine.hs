-- |
-- Module      : Xine
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL (see COPYING)
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
-- >     sid <- Xine.openStream h "track.mp3"
--
-- >     Xine.play h sid

module Xine (
    -- * Configuration
    XineConf(..), VisualType(..), defaultConf,
    -- * Handle
    XineHandle, open, openWith, close, isClosed,
    -- * Streams
    -- $streams
    MRL, StreamId,
    openStream, closeStream, getCurrent,
    -- * Playback
    SeekArg(..),
    play, seek, stop, pause,
    -- * Information retrieval
    EngineStatus(..), MetaType(..),
    getStatus, getMetadata
    ) where

import Xine.Foreign
import Xine.Internal.Handle
import Xine.Internal.Stream (StreamId)
import qualified Xine.Internal.Stream as S

import Control.Concurrent.MVar
import Control.Monad (unless)
import Data.Maybe (fromJust)

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | Xine configuration.
data XineConf = XineConf
    { audioDriver :: !(Maybe String)
      -- ^ Audio driver. Use 'Nothing' for
      -- auto-detection.
    , videoDriver :: !(Maybe String)
      -- ^ Video driver. Use 'Nothing' for
      -- auto-detection.
    , visualType :: !VisualType
      -- ^ Video output type. Use 'None' to disable video output.
    }

-- | Default configuration. Audio only.
defaultConf :: XineConf
defaultConf = XineConf
    { audioDriver = Nothing
    , videoDriver = Nothing
    , visualType = None
    }

------------------------------------------------------------------------------
-- Handle
------------------------------------------------------------------------------

-- | Open a new Xine handle using 'defaultConf'.
open :: IO XineHandle
open = openWith defaultConf

-- | Open a new Xine handle using the supplied 'XineConf'.
openWith :: XineConf -> IO XineHandle
openWith conf = do
    engine <- xine_new
    xine_init engine

    ap <- maybe (fail "Failed to open the audio driver") return =<<
          xine_open_audio_driver engine (audioDriver conf)
    vp <- maybe (fail "Failed to open the video driver") return =<<
          xine_open_video_driver engine (videoDriver conf) (visualType conf)

    h_ <- newMVar $ XineHandle_ engine ap vp S.empty Nothing Open
    return $ XineHandle h_

-- | Close Xine handle. The handle is invalid after this.
close :: XineHandle -> IO ()
close h@(XineHandle hv) = do
    withXineHandle h $ \h_ -> do
        mapM_ xine_close (S.streams $ hStreams h_)
        xine_close_audio_driver (hEngine h_) (hAudioPort h_)
        xine_close_video_driver (hEngine h_) (hVideoPort h_)
        xine_exit (hEngine h_)
    modifyMVar_ hv $ \x -> return x { hState = Closed }

------------------------------------------------------------------------------
-- $streams
--
-- You may open multiple streams for playback. Beware, though, that new
-- streams cannot be added once playback has started, so open your streams
-- before using them.
------------------------------------------------------------------------------

-- | Open a new stream for the given MRL.
openStream :: XineHandle -> MRL -> IO StreamId
openStream h uri = do
    modifyXineHandle h $ \h_ -> do
        -- Create a new stream
        st <- maybe (fail "Failed to open a new stream") return =<<
              xine_stream_new (hEngine h_) (hAudioPort h_) (hVideoPort h_)

        -- Open the MRL
        ret <- xine_open st uri
        unless (ret == 1) (fail "Failed to open MRL")

        -- Add the stream to the handle
        let (s, i) = S.insert st (hStreams h_)
        return $ h_ { hStreams = s
                    , hCurrent = Just i }

    fromJust `fmap` getCurrent h

-- | Close the specified stream.
closeStream :: XineHandle -> StreamId -> IO ()
closeStream h sid = modifyXineHandle h $ \h_ ->
    case S.lookup sid (hStreams h_) of
        Just st -> do
            xine_close st
            return $ h_ { hStreams = S.delete sid (hStreams h_) }
        Nothing -> return h_

-- | Get the current stream, if any.
getCurrent :: XineHandle -> IO (Maybe StreamId)
getCurrent h = withXineHandle h $ \hv -> return (hCurrent hv)

------------------------------------------------------------------------------
-- Playback
------------------------------------------------------------------------------

-- | Start playback.
play :: XineHandle -> StreamId -> IO ()
play h sid = withStream h sid $ \st -> do
    ret <- xine_play st 0 0
    unless (ret == 1) (fail "Failed to start playback")

-- | Argument for 'seek'.
data SeekArg
    = SeekTime Int
    | SeekPos Int
      deriving (Eq, Show)

-- | Seek to a position or time in the stream.
--
-- Warning: this will crash if 'xine_trick_mode' is not implemented.
seek :: XineHandle -> StreamId -> SeekArg -> IO ()
seek h sid arg = withStream h sid $ \st -> do
    ret <- xine_trick_mode st (trick arg) (val arg)
    unless (ret == 1) (fail "Seek failed")
    where
        val (SeekTime x) = x
        val (SeekPos x) = x

        trick (SeekTime _) = TrickSeekToTime
        trick (SeekPos _)  = TrickSeekToPosition

-- | Stop playback.
stop :: XineHandle -> StreamId -> IO ()
stop h sid = withStream h sid $ \st -> xine_stop st

-- | Toggle pause.
pause :: XineHandle -> StreamId -> IO ()
pause h sid = withStream h sid $ \st -> do
    s <- xine_get_param st Speed
    let speed | s == Pause = Normal
              | otherwise  = Pause
    xine_set_param st Speed speed

------------------------------------------------------------------------------
-- Information retrieval
------------------------------------------------------------------------------

-- | Get current engine status.
getStatus :: XineHandle -> IO EngineStatus
getStatus h = withXineHandle h $ \h_ ->
    case hCurrent h_ of
        Just sid -> xine_get_status (fromJust $ S.lookup sid (hStreams h_))
        Nothing  -> return Idle

-- | Get meta data about the given stream.
getMetadata :: XineHandle -> StreamId -> MetaType -> IO String
getMetadata h sid m = withStream h sid $ \st -> xine_get_meta_info st m
