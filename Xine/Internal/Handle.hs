-- |
-- Module      : Xine.Internal.Handle
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL (see COPYING)
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- A 'Handle'-like interface for a Xine engine instance.

module Xine.Internal.Handle (
   -- * Xine engine handle
   -- XXX: add additional operators and helpers so we do not have
   -- to expose the underlying implementation to the consumer.
   HandleState(..), XineHandle_(..), XineHandle(..), isClosed,
   -- * Using handles
   modifyXineHandle, withXineHandle, withStream
   ) where

import Xine.Foreign
import Xine.Internal.Stream (Streams, StreamId)
import qualified Xine.Internal.Stream as S

import Control.Concurrent.MVar
import Control.Monad (when)

data HandleState = Closed | Open deriving Eq

data XineHandle_ = XineHandle_
    { hEngine :: !Engine
    , hAudioPort :: !AudioPort
    , hVideoPort :: !VideoPort
    , hStreams :: !Streams
    , hCurrent :: !(Maybe StreamId)
    , hState :: !HandleState
    }

-- | A xine-lib handle.
newtype XineHandle = XineHandle (MVar XineHandle_)

-- | Test whether the handle is closed.
isClosed :: XineHandle -> IO Bool
isClosed (XineHandle hv) = withMVar hv $ \h -> return (hState h == Closed)

-- | A helper for modifying the internal handle state.
modifyXineHandle :: XineHandle -> (XineHandle_ -> IO XineHandle_) -> IO ()
modifyXineHandle h@(XineHandle hv) f = do
    closed <- isClosed h
    when closed (fail "XineHandle is closed")
    modifyMVar_ hv f

-- | A helper for functions using the xine-handle wrapper.
withXineHandle :: XineHandle -> (XineHandle_ -> IO a) -> IO a
withXineHandle h@(XineHandle hv) f = do
    closed <- isClosed h
    when closed (fail "XineHandle is closed")
    withMVar hv f

-- | A helper for using a given stream.
withStream :: XineHandle -> StreamId -> (Stream -> IO a) -> IO a
withStream h sid f = withXineHandle h $ \hv ->
    case S.lookup sid (hStreams hv) of
        Just s -> f s
        Nothing -> fail $ "No such stream: " ++ show sid
