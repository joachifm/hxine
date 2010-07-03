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
    play, seek, stop, pause
    ) where

import Xine.Foreign

import Control.Concurrent.MVar
import Control.Monad (unless, when)
import qualified Data.Map as M
import Data.Maybe (fromJust)

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
    , hStreams :: !Streams
    , hCurrent :: !(Maybe StreamId)
    , hState :: !HandleState
    }

-- | Identifies an open stream.
type StreamId = Int

-- A container for streams.
data Streams = Streams
    { mapping :: !(M.Map StreamId Stream)
    , lastKey :: !StreamId
    }

emptyStreams :: Streams
emptyStreams = Streams M.empty 0

addStream :: Stream -> Streams -> Streams
addStream x s =
    let nextId = succ (lastKey s)
    in s { mapping = M.insert nextId x (mapping s)
         , lastKey = nextId }

delStream :: StreamId -> Streams -> Streams
delStream sid s = s { mapping = M.delete sid (mapping s) }

lookupStream :: StreamId -> Streams -> Maybe Stream
lookupStream sid s = M.lookup sid (mapping s)

streams :: Streams -> [Stream]
streams = M.elems . mapping

-- | A xine-lib handle.
newtype XineHandle = XineHandle (MVar XineHandle_)

-- | Test whether the handle is closed.
isClosed :: XineHandle -> IO Bool
isClosed (XineHandle hv) = withMVar hv $ \h -> return (hState h == Closed)

-- A helper for modifying the internal handle state.
modifyXineHandle :: XineHandle -> (XineHandle_ -> IO XineHandle_) -> IO ()
modifyXineHandle h@(XineHandle hv) f = do
    closed <- isClosed h
    when closed (fail "XineHandle is closed")
    modifyMVar_ hv f

-- A helper for functions using the xine-handle wrapper.
withXineHandle :: XineHandle -> (XineHandle_ -> IO a) -> IO a
withXineHandle h@(XineHandle hv) f = do
    closed <- isClosed h
    when closed (fail "XineHandle is closed")
    withMVar hv $ f

-- A helper for using a given stream.
withStream :: XineHandle -> StreamId -> (Stream -> IO a) -> IO a
withStream h sid f = withXineHandle h $ \hv -> do
    case lookupStream sid (hStreams hv) of
        Just s -> f s
        Nothing -> fail $ "No such stream: " ++ show sid

-- | Open a new Xine handle using the default configuration.
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

    h_ <- newMVar $ XineHandle_ engine ap vp emptyStreams Nothing Open
    return $ XineHandle h_

-- | Close Xine handle. The handle is invalid after this.
close :: XineHandle -> IO ()
close h@(XineHandle hv) = do
    withXineHandle h $ \h_ -> do
        mapM_ xine_close (streams $ hStreams h_)
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

-- | Open an new stream for the given MRL.
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
        let sm = addStream st (hStreams h_)
        return $ h_ { hStreams = sm
                    , hCurrent = Just (lastKey sm) }
    fromJust `fmap` getCurrent h

-- | Close the specified stream.
closeStream :: XineHandle -> StreamId -> IO ()
closeStream h sid = modifyXineHandle h $ \h_ -> do
    case lookupStream sid (hStreams h_) of
        Just st -> do
            xine_close st
            return $ h_ { hStreams = delStream sid (hStreams h_) }
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
