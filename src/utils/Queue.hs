module Queue
  ( WriteQueue(..)
  , writeQueue
  , ReadQueue(..)
  , readQueue
  , flushQueue
  ) where

import Control.Concurrent.STM

newtype WriteQueue a = WriteQueue
  { getWriteQueue :: TQueue a
  }

{-# INLINE writeQueue #-}
writeQueue :: WriteQueue a -> a -> STM ()
writeQueue = writeTQueue . getWriteQueue

newtype ReadQueue a = ReadQueue
  { getReadQueue :: TQueue a
  }

{-# INLINE readQueue #-}
readQueue :: ReadQueue a -> STM a
readQueue = readTQueue . getReadQueue

{-# INLINE flushQueue #-}
flushQueue :: ReadQueue a -> STM [a]
flushQueue = flushTQueue . getReadQueue
