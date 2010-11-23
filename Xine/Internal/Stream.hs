-- |
-- Module      : Xine.Internal.Stream
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL (see COPYING)
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- Code for supporting multiple Xine input streams.

module Xine.Internal.Stream (
    -- * The stream container type
    StreamId, Streams, empty, streams,
    -- * Stream container operators
    insert, delete, lookup
    ) where

import Xine.Foreign (Stream)

import Prelude hiding (lookup)
import qualified Data.Map as M

-- | Identifies an open stream.
type StreamId = Int

-- | A container for streams.
--
-- This is a 'Map StreamId Stream' that stores the last id. Each new id is
-- the succ of the last id, making them unique within the map, even when items
-- are deleted. The uniqueness property allows us to juggle stream identifiers
-- around without having to keep track of deletes (i.e., we don't have to carry
-- the actual mapping around with us).
data Streams = Streams
    { mapping :: !(M.Map StreamId Stream)
    , lastKey :: !StreamId
    }

-- | An empty container.
empty :: Streams
empty = Streams M.empty 0

-- | Add a stream, returning a new container along with the id of the
-- recently added stream.
insert :: Stream -> Streams -> (Streams, StreamId)
insert x s =
    let nextId = succ (lastKey s)
        s'     = s { mapping = M.insert nextId x (mapping s)
                   , lastKey = nextId }
    in (s', nextId)

-- | Remove the identified stream.
delete :: StreamId -> Streams -> Streams
delete sid s = s { mapping = M.delete sid (mapping s) }

-- | Stream lookup.
lookup :: StreamId -> Streams -> Maybe Stream
lookup sid s = M.lookup sid (mapping s)

-- | Return a list of streams in the container.
streams :: Streams -> [Stream]
streams = M.elems . mapping
